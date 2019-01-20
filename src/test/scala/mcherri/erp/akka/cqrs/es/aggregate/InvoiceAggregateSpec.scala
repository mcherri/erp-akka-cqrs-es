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
import mcherri.erp.akka.cqrs.es.aggregate.InvoiceAggregate.Protocol._
import mcherri.erp.akka.cqrs.es.aggregate.RestartableActor.RestartActor
import mcherri.erp.akka.cqrs.es.model.Invoice.EmptyInvoiceError
import mcherri.erp.akka.cqrs.es.model.Invoice.Protocol.{InvoiceCanceled, InvoiceCreated, ItemsAdded}
import mcherri.erp.akka.cqrs.es.model._
import org.scalactic._

// Only direct scenarios will be test here as indirect ones where already tested in InvoiceSpec.
class InvoiceAggregateSpec extends ActorUnitSpec {

  trait InvoiceAggregateFixture extends InvoiceFixture {
    protected val uninitializedAggregate: Or[ActorRef, Every[Error]] =
      id.map(id => system.actorOf(Props(new InvoiceAggregate with RestartableActor), id.value.toString))

    protected val initializeAggregate: Or[ActorRef, Every[Error]] = for (
      aggregate <- uninitializedAggregate;
      invoiceId <- id;
      client <- client
    ) yield {
      aggregate ! CreateInvoice(client)
      expectMsgPF() {
        case Good(InvoiceCreated(`invoiceId`, `client`)) => Unit
      }
      aggregate
    }
  }

  "An invoice aggregate" should "respond to AddItems request with ItemsAdded" in new InvoiceAggregateFixture {
    for (
      invoiceId <- id;
      aggregate <- initializeAggregate;
      lineItems <- lineItemSeq
    ) {
      aggregate ! AddItems(lineItems)
      expectMsgPF() {
        case Good(ItemsAdded(`invoiceId`, `lineItems`)) => Unit
      }
    }
  }

  it should "respond to DeleteItems with non-existing items with an error" in new InvoiceAggregateFixture {
    for (
      aggregate <- initializeAggregate;
      itemIds <- itemIdsToDelete
    ) {
      aggregate ! DeleteItems(itemIds)
      expectMsgPF() {
        case Bad(One(_: EmptyInvoiceError)) => Unit
      }
    }
  }

  it should "respond to CancelInvoice with existing items with InvoiceCanceled" in new InvoiceAggregateFixture {
    for (
      invoiceId <- id;
      aggregate <- initializeAggregate
    ) {
      aggregate ! CancelInvoice()
      expectMsgPF() {
        case Good(InvoiceCanceled(`invoiceId`)) => Unit
      }
    }
  }

  it should "respond to IssueInvoice with no items with an error" in new InvoiceAggregateFixture {
    for (
      aggregate <- initializeAggregate
    ) {
      aggregate ! IssueInvoice()
      expectMsgPF() {
        case Bad(One(_: EmptyInvoiceError)) => Unit
      }
    }
  }

  it should "restore its states after unexpected errors" in new InvoiceAggregateFixture {
    for (
      aggregate <- initializeAggregate;
      lineItems <- lineItemSeq
    ) {

      // Now restart and get the state
      aggregate ! RestartActor
      aggregate ! GetState
      expectMsgPF() {
        case Good(EmptyInvoice(_, _)) => Unit // The state should be the good one that was persisted
      }

      aggregate ! AddItems(lineItems)
      expectMsgPF() {
        case Good(ItemsAdded(_, _)) => Unit
      }

      // Restart again
      aggregate ! RestartActor
      aggregate ! GetState
      expectMsgPF() {
        case Good(DraftInvoice(_, _, _)) => Unit // The state should be the good one that was persisted
      }
    }
  }
}
