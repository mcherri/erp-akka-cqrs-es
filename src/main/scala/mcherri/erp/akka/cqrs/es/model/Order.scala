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
package mcherri.erp.akka.cqrs.es.model

import java.util.UUID

import mcherri.erp.akka.cqrs.es.model.LineItem.LineItemError
import mcherri.erp.akka.cqrs.es.model.Order.Protocol._
import mcherri.erp.akka.cqrs.es.model.Order._
import mcherri.erp.akka.cqrs.es.model.OrderId.OrderIdError
import mcherri.erp.akka.cqrs.es.utils.RichOr._
import org.scalactic.Accumulation._
import org.scalactic._
import org.sisioh.baseunits.scala.money.Money

import scala.collection.immutable.Seq

abstract case class OrderId private[OrderId](id: UUID) extends UuidAggregateId {
  def copy(id: UUID = id): OrderId Or Every[OrderIdError] = OrderId.apply(id)
}

object OrderId {

  def apply(id: UUID): OrderId Or Every[OrderIdError] = {
    if (id.version() == 4) {
      Good(new OrderId(id) {})
    } else {
      Bad(One(UuidError(id)))
    }
  }

  sealed abstract class OrderIdError(message: String) extends Error(message)

  case class UuidError(id: UUID)
    extends OrderIdError(s"OrderId with UUID = $id is not version 4 UUID")

}

abstract case class LineItem private[LineItem](itemId: ItemId, code: String,
                                               price: Money, quantity: BigDecimal) {
  def copy(itemId: ItemId = itemId, code: String = code, price: Money = price,
           quantity: BigDecimal = quantity): LineItem Or Every[LineItemError] =
    LineItem.apply(itemId, code, price, quantity)
}

object LineItem {

  def apply(itemId: ItemId, code: String, price: Money, quantity: BigDecimal): LineItem Or Every[LineItemError] = {
    val priceOrError: Or[Money, One[NegativePriceError]] = if (price.isNegative) {
      Bad(One(NegativePriceError(price)))
    } else {
      Good(price)
    }
    val quantityOrError = quantity match {
      case q if q <= 0 => Bad(One(NegativeOrZeroQuantityError(quantity)))
      //      case q if q > item.remainingQuantity => Bad(One(MoreThanRemainingQuantityError(quantity)))
      case q => Good(q)
    }

    withGood(priceOrError, quantityOrError) { (price, quantity) =>
      new LineItem(itemId, code, price, quantity) {}
    }
  }

  sealed abstract class LineItemError(message: String) extends Error(message)

  case class NegativePriceError(price: Money)
    extends LineItemError(s"Line item with negative price $price")

  case class NegativeOrZeroQuantityError(quantity: BigDecimal)
    extends LineItemError(s"Line item with negative or zero quantity $quantity")

  case class MoreThanRemainingQuantityError(quantity: BigDecimal)
    extends LineItemError(s"Line item with quantity that is more than the item remaining quantity")

}

/*
 * The code below implements order states using the workflow below:
 *
 *                                                Add
 *            +---------------+ Init  +-------+  Items   +-------------+
 * Start +--->+ Uninitialized +------>+ Empty +--------->+    Draft    |
 *            +---------------+       +-+---+-+          +----+----+-+-+
 *                                      |   ^    Delete       |    | |
 *                                      |   |    Items        |    | |
 *                                      |   +-----------------+    | |
 *                                      |                          | |
 *                                      |   +----------+   Cancel  | |
 *                                      +-->+ Canceled +<----------+ |
 *                                          +----------+             |
 *                                                ^                  |
 *                                         Cancel |                  |
 *                                          +-----+----+     Issue   |
 *                                End <-----+  Issued  +<------------+
 *                                          +----------+
 *
 * P.S: No Order case class is needed in this case.
 */
trait OrderState extends State {
  override type StateOrErrors = OrderState Or Every[Error]
  override type DomainEvent = OrderDomainEvent

  def canInit(id: OrderId, client: Client): DomainEventsOrErrors

  def init(id: OrderId, client: Client): StateOrErrors

  def canAdd(newLines: Seq[LineItem]): DomainEventsOrErrors

  def add(newLines: Seq[LineItem]): StateOrErrors

  def canDelete(itemIds: Seq[ItemId]): DomainEventsOrErrors

  def delete(itemIds: Seq[ItemId]): StateOrErrors

  def canCancel: DomainEventsOrErrors

  def cancel(): StateOrErrors

  def canIssue: DomainEventsOrErrors

  def issue(): StateOrErrors
}

abstract class AbstractOrderState(defaultErrors: Every[OrderError]) extends OrderState {
  override def canInit(id: OrderId, client: Client): DomainEventsOrErrors = Bad(defaultErrors)

  override def init(id: OrderId, client: Client): StateOrErrors = Bad(defaultErrors)

  override def canAdd(newLines: Seq[LineItem]): DomainEventsOrErrors = Bad(defaultErrors)

  override def add(newLines: Seq[LineItem]): StateOrErrors = Bad(defaultErrors)

  override def canDelete(itemIds: Seq[ItemId]): DomainEventsOrErrors = Bad(defaultErrors)

  override def delete(itemIds: Seq[ItemId]): StateOrErrors = Bad(defaultErrors)

  override def canCancel: DomainEventsOrErrors = Bad(defaultErrors)

  override def cancel(): StateOrErrors = Bad(defaultErrors)

  override def canIssue: DomainEventsOrErrors = Bad(defaultErrors)

  override def issue(): StateOrErrors = Bad(defaultErrors)
}

case object UninitializedOrder extends AbstractOrderState(One(Order.UninitializedOrderError)) {
  override def init(id: OrderId, client: Client): StateOrErrors = {
    canInit(id, client).map(_ => EmptyOrder(id, client))
  }

  // TODO: Maybe we need to validate the parameters here.
  override def canInit(id: OrderId, client: Client): DomainEventsOrErrors =
    Good(Seq(OrderCreated(id, client)))
}

case class EmptyOrder(id: OrderId, client: Client) extends AbstractOrderState(One(EmptyOrderError(id))) {
  override def canInit(id: OrderId, client: Client): UninitializedOrder.DomainEventsOrErrors = Bad(One(AlreadyInitializedError))

  override def init(id: OrderId, client: Client): StateOrErrors = Bad(One(AlreadyInitializedError))

  override def add(newLines: Seq[LineItem]): StateOrErrors = canAdd(newLines).map { _ =>
    if (newLines.isEmpty) {
      this
    } else {
      DraftOrder(id, client, newLines)
    }
  }

  override def canAdd(newLines: Seq[LineItem]): DomainEventsOrErrors = Good(Seq(ItemsAdded(id, newLines)))

  override def cancel(): StateOrErrors = canCancel.map(_ => CanceledOrder(id))

  override def canCancel: DomainEventsOrErrors = Good(Seq(OrderCanceled(id)))
}

case class DraftOrder(id: OrderId, client: Client,
                      itemLines: Seq[LineItem]) extends AbstractOrderState(One(AlreadyInitializedError)) {
  override def add(newLines: Seq[LineItem]): StateOrErrors =
    for (
      _ <- canAdd(newLines);
      itemLines <- merge(newLines)
    ) yield copy(itemLines = itemLines)

  override def canAdd(newLines: Seq[LineItem]): DomainEventsOrErrors = Good(Seq(ItemsAdded(id, newLines)))

  private def merge(newLines: Seq[LineItem]) = {
    (itemLines ++ newLines).groupBy(_.itemId).map {
      case (_, seq: Seq[LineItem]) =>
        seq.tail.foldLeft(Good(seq.head): LineItem Or Every[LineItemError]) { (acc, line1) =>
          acc.flatMap(line2 => line2.copy(quantity = line1.quantity + line2.quantity))
        }
    }.to[Seq].sequence()
  }

  override def delete(itemIds: Seq[ItemId]): StateOrErrors = {
    val newItemLines = itemLines.filter(lineItem => !itemIds.contains(lineItem.itemId))

    canDelete(itemIds).map { _ =>
      if (newItemLines.isEmpty) {
        EmptyOrder(id, client)
      } else {
        copy(itemLines = newItemLines)
      }
    }
  }

  override def canDelete(itemIds: Seq[ItemId]): DomainEventsOrErrors = {
    val newItemLines = itemLines.filter(lineItem => !itemIds.contains(lineItem.itemId))

    if (newItemLines.size < itemLines.size) {
      Good(Seq(ItemsDeleted(id, itemIds)))
    } else {
      // TODO: Identify which item was not found exactly
      Bad(One(ItemIdsNotFoundError(id, itemIds)))
    }

  }

  override def cancel(): StateOrErrors = canCancel.map(_ => CanceledOrder(id))

  override def canCancel: DomainEventsOrErrors = Good(Seq(OrderCanceled(id)))

  override def issue(): StateOrErrors = canIssue.map(_ => IssuedOrder(id))

  override def canIssue: DomainEventsOrErrors = Good(Seq(OrderIssued(id)))
}

case class CanceledOrder(id: OrderId) extends AbstractOrderState(One(AlreadyCanceledError(id)))

case class IssuedOrder(id: OrderId) extends AbstractOrderState(One(AlreadyIssuedError(id))) {
  override def cancel(): StateOrErrors = canCancel.map(_ => CanceledIssuedOrder(id))

  override def canCancel: DomainEventsOrErrors = Good(Seq(OrderCanceled(id)))
}

case class CanceledIssuedOrder(id: OrderId) extends
  AbstractOrderState(Many(AlreadyCanceledError(id), AlreadyIssuedError(id)))

object Order {

  sealed abstract class OrderError(message: String) extends Error(message)

  case class ItemIdsNotFoundError(id: OrderId, itemIds: Seq[ItemId])
    extends OrderError(s"Order with id = $id does not contain one of these item ids (${itemIds.mkString(", ")})")

  case class AlreadyCanceledError(id: OrderId)
    extends OrderError(s"Order with id = $id does is already canceled")

  case class AlreadyIssuedError(id: OrderId)
    extends OrderError(s"Order with id = $id does is already issued")

  case class EmptyOrderError(id: OrderId)
    extends OrderError(s"Order with id = $id does is empty")

  case object UninitializedOrderError extends OrderError("Order is not initialized yet")

  case object AlreadyInitializedError extends OrderError("Order is already initialized")

  object Protocol {

    sealed trait OrderDomainEvent extends DomainEvent

    case class OrderCreated(id: OrderId, client: Client) extends OrderDomainEvent

    case class ItemsAdded(id: OrderId, lineItems: Seq[LineItem]) extends OrderDomainEvent

    case class ItemsDeleted(id: OrderId, itemIds: Seq[ItemId]) extends OrderDomainEvent

    case class OrderCanceled(id: OrderId) extends OrderDomainEvent

    case class OrderIssued(id: OrderId) extends OrderDomainEvent

  }

}
