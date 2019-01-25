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

import java.util.UUID

import mcherri.erp.akka.cqrs.es.aggregate.OrderAggregate.Protocol._
import mcherri.erp.akka.cqrs.es.model.Order.Protocol._
import mcherri.erp.akka.cqrs.es.model._
import org.scalactic.Good

import scala.collection.immutable.Seq

class OrderAggregate extends Aggregate[OrderState] {
  state = Good(UninitializedOrder)

  override protected def applyCommand(command: Command): CommandResult =
    for (
      state1 <- state;
      event <- command match {
        case CreateOrder(client) => OrderId(UUID.fromString(id)).flatMap(state1.canInit(_, client))
        case AddItems(_, lineItems) => state1.canAdd(lineItems)
        case DeleteItems(_, itemIds) => state1.canDelete(itemIds)
        case CancelOrder(_) => state1.canCancel
        case IssueOrder(_) => state1.canIssue
      }
    ) yield Seq(event) // FIXME: We should not wrap with Seq

  override protected def applyEvent(event: DomainEvent): StateOrError =
    for (
      state1 <- state;
      state2 <- event match {
        case OrderCreated(id, client) => state1.init(id, client)
        case ItemsAdded(_, lineItems) => state1.add(lineItems)
        case ItemsDeleted(_, itemIds) => state1.delete(itemIds)
        case OrderCanceled(_) => state1.cancel()
        case OrderIssued(_) => state1.issue()
      }
    ) yield state2
}

object OrderAggregate {

  object Protocol {

    sealed trait OrderCommand extends UpdateCommand

    case class CreateOrder(client: Client) extends CreateCommand

    case class AddItems(id: OrderId, lineItems: Seq[LineItem]) extends OrderCommand

    case class DeleteItems(id: OrderId, itemIds: Seq[ItemId]) extends OrderCommand

    case class CancelOrder(id: OrderId) extends OrderCommand

    case class IssueOrder(id: OrderId) extends OrderCommand
  }

}
