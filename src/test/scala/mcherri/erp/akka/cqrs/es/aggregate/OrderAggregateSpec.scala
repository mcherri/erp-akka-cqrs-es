/*
 * Copyright © 2019 Mustapha Cherri
 *
 * This file is part of erp-akka-cqrs-es.
 *
 * erp-akka-cqrs-es is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * erp-akka-cqrs-es is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with erp-akka-cqrs-es.  If not, see <https://www.gnu.org/licenses/>.
 */
package mcherri.erp.akka.cqrs.es.aggregate

import akka.actor.{ActorRef, Props}
import mcherri.erp.akka.cqrs.es.ActorUnitSpec
import mcherri.erp.akka.cqrs.es.aggregate.Aggregate.Protocol.GetState
import mcherri.erp.akka.cqrs.es.aggregate.OrderAggregate.Protocol._
import mcherri.erp.akka.cqrs.es.aggregate.RestartableActor.RestartActor
import mcherri.erp.akka.cqrs.es.model.Order.EmptyOrderError
import mcherri.erp.akka.cqrs.es.model.Order.Protocol.{OrderCanceled, OrderCreated, ItemsAdded}
import mcherri.erp.akka.cqrs.es.model._
import org.scalactic._

// Only direct scenarios will be test here as indirect ones where already tested in OrderSpec.
class OrderAggregateSpec extends ActorUnitSpec {

  trait OrderAggregateFixture extends OrderFixture {
    protected val uninitializedAggregate: Or[ActorRef, Every[Error]] =
      id.map(id => system.actorOf(Props(new OrderAggregate with RestartableActor), id.value))

    protected val initializeAggregate: Or[ActorRef, Every[Error]] = for (
      aggregate <- uninitializedAggregate;
      orderId <- id;
      client <- client
    ) yield {
      aggregate ! CreateOrder(client)
      expectMsgPF() {
        case Good(OrderCreated(`orderId`, `client`)) => Unit
      }
      aggregate
    }
  }

  "An order aggregate" should "respond to AddItems request with ItemsAdded" in new OrderAggregateFixture {
    for (
      orderId <- id;
      aggregate <- initializeAggregate;
      lineItems <- lineItemSeq
    ) {
      aggregate ! AddItems(lineItems)
      expectMsgPF() {
        case Good(ItemsAdded(`orderId`, `lineItems`)) => Unit
      }
    }
  }

  it should "respond to DeleteItems with non-existing items with an error" in new OrderAggregateFixture {
    for (
      aggregate <- initializeAggregate;
      itemIds <- itemIdsToDelete
    ) {
      aggregate ! DeleteItems(itemIds)
      expectMsgPF() {
        case Bad(One(_: EmptyOrderError)) => Unit
      }
    }
  }

  it should "respond to CancelOrder with existing items with OrderCanceled" in new OrderAggregateFixture {
    for (
      orderId <- id;
      aggregate <- initializeAggregate
    ) {
      aggregate ! CancelOrder()
      expectMsgPF() {
        case Good(OrderCanceled(`orderId`)) => Unit
      }
    }
  }

  it should "respond to IssueOrder with no items with an error" in new OrderAggregateFixture {
    for (
      aggregate <- initializeAggregate
    ) {
      aggregate ! IssueOrder()
      expectMsgPF() {
        case Bad(One(_: EmptyOrderError)) => Unit
      }
    }
  }

  it should "restore its states after unexpected errors" in new OrderAggregateFixture {
    for (
      aggregate <- initializeAggregate;
      lineItems <- lineItemSeq
    ) {

      // Now restart and get the state
      aggregate ! RestartActor
      aggregate ! GetState
      expectMsgPF() {
        case Good(EmptyOrder(_, _)) => Unit // The state should be the good one that was persisted
      }

      aggregate ! AddItems(lineItems)
      expectMsgPF() {
        case Good(ItemsAdded(_, _)) => Unit
      }

      // Restart again
      aggregate ! RestartActor
      aggregate ! GetState
      expectMsgPF() {
        case Good(DraftOrder(_, _, _)) => Unit // The state should be the good one that was persisted
      }
    }
  }
}
