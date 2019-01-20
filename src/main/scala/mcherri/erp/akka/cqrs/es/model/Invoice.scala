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

import mcherri.erp.akka.cqrs.es.model.Invoice.Protocol._
import mcherri.erp.akka.cqrs.es.model.Invoice._
import mcherri.erp.akka.cqrs.es.model.InvoiceId.InvoiceIdError
import mcherri.erp.akka.cqrs.es.model.LineItem.LineItemError
import org.scalactic.Accumulation._
import org.scalactic._
import org.sisioh.baseunits.scala.money.Money
import mcherri.erp.akka.cqrs.es.utils.RichOr._

import scala.collection.immutable.Seq

abstract case class InvoiceId private[InvoiceId](value: UUID) {
  def copy(value: UUID = value): InvoiceId Or Every[InvoiceIdError] = InvoiceId.apply(value)
}

object InvoiceId {

  def apply(value: UUID): InvoiceId Or Every[InvoiceIdError] = {
    if (value.version() == 4) {
      Good(new InvoiceId(value) {})
    } else {
      Bad(One(UuidError(value)))
    }
  }

  sealed abstract class InvoiceIdError(message: String) extends Error(message)

  case class UuidError(value: UUID)
    extends InvoiceIdError(s"InvoiceId with UUID = $value is not version 4 UUID")
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
 * The code below implements invoice states using the workflow below:
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
 * P.S: No Invoice case class is needed in this case.
 */
trait InvoiceState extends State {
  override type StateOrErrors = InvoiceState Or Every[Error]
  override type DomainEventOrErrors = InvoiceDomainEvent Or Every[Error]

  def canInit(id: InvoiceId, client: Client): DomainEventOrErrors

  def init(id: InvoiceId, client: Client): StateOrErrors

  def canAdd(newLines: Seq[LineItem]): DomainEventOrErrors

  def add(newLines: Seq[LineItem]): StateOrErrors

  def canDelete(itemIds: Seq[ItemId]): DomainEventOrErrors

  def delete(itemIds: Seq[ItemId]): StateOrErrors

  def canCancel: DomainEventOrErrors

  def cancel(): StateOrErrors

  def canIssue: DomainEventOrErrors

  def issue(): StateOrErrors
}

// FIXME: AbstractInvoiceState should take an Every[InvoiceError]
abstract class AbstractInvoiceState(defaultError: InvoiceError) extends InvoiceState {
  override def canInit(id: InvoiceId, client: Client): DomainEventOrErrors = Bad(One(defaultError))

  override def init(id: InvoiceId, client: Client): StateOrErrors = Bad(One(defaultError))

  override def canAdd(newLines: Seq[LineItem]): DomainEventOrErrors = Bad(One(defaultError))

  override def add(newLines: Seq[LineItem]): StateOrErrors = Bad(One(defaultError))

  override def canDelete(itemIds: Seq[ItemId]): DomainEventOrErrors = Bad(One(defaultError))

  override def delete(itemIds: Seq[ItemId]): StateOrErrors = Bad(One(defaultError))

  override def canCancel: DomainEventOrErrors = Bad(One(defaultError))

  override def cancel(): StateOrErrors = Bad(One(defaultError))

  override def canIssue: DomainEventOrErrors = Bad(One(defaultError))

  override def issue(): StateOrErrors = Bad(One(defaultError))
}

case object UninitializedInvoice extends AbstractInvoiceState(Invoice.UninitializedInvoiceError) {
  // TODO: Maybe we need to validate the parameters here.
  override def canInit(id: InvoiceId, client: Client): DomainEventOrErrors =
    Good(InvoiceCreated(id, client))

  override def init(id: InvoiceId, client: Client): StateOrErrors = {
    canInit(id, client).map(_ => EmptyInvoice(id, client))
  }
}

case class EmptyInvoice(id: InvoiceId, client: Client) extends AbstractInvoiceState(EmptyInvoiceError(id)) {
  override def canInit(id: InvoiceId, client: Client): UninitializedInvoice.DomainEventOrErrors = Bad(One(AlreadyInitializedError))

  override def init(id: InvoiceId, client: Client): StateOrErrors = Bad(One(AlreadyInitializedError))

  override def add(newLines: Seq[LineItem]): StateOrErrors = canAdd(newLines).map { _ =>
    if (newLines.isEmpty) {
      this
    } else {
      DraftInvoice(id, client, newLines)
    }
  }

  override def canAdd(newLines: Seq[LineItem]): DomainEventOrErrors = Good(ItemsAdded(id, newLines))

  override def canCancel: DomainEventOrErrors = Good(InvoiceCanceled(id))

  override def cancel(): StateOrErrors = canCancel.map(_ => CanceledInvoice(id))
}

case class DraftInvoice(id: InvoiceId, client: Client,
                        itemLines: Seq[LineItem]) extends AbstractInvoiceState(AlreadyInitializedError) {
  override def add(newLines: Seq[LineItem]): StateOrErrors =
    for (
      _ <- canAdd(newLines);
      itemLines <- merge(newLines)
    ) yield copy(itemLines = itemLines)

  override def canAdd(newLines: Seq[LineItem]): DomainEventOrErrors = Good(ItemsAdded(id, newLines))

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
        EmptyInvoice(id, client)
      } else {
        copy(itemLines = newItemLines)
      }
    }
  }

  override def canDelete(itemIds: Seq[ItemId]): DomainEventOrErrors = {
    val newItemLines = itemLines.filter(lineItem => !itemIds.contains(lineItem.itemId))

    if (newItemLines.size < itemLines.size) {
      Good(ItemsDeleted(id, itemIds))
    } else {
      // TODO: Identify which item was not found exactly
      Bad(One(ItemIdsNotFoundError(id, itemIds)))
    }

  }

  override def cancel(): StateOrErrors = canCancel.map(_ => CanceledInvoice(id))

  override def canCancel: DomainEventOrErrors = Good(InvoiceCanceled(id))

  override def issue(): StateOrErrors = canIssue.map(_ => IssuedInvoice(id))

  override def canIssue: DomainEventOrErrors = Good(InvoiceIssued(id))
}

case class CanceledInvoice(id: InvoiceId) extends AbstractInvoiceState(AlreadyCanceledError(id))

case class IssuedInvoice(id: InvoiceId) extends AbstractInvoiceState(AlreadyIssuedError(id)) {
  override def cancel(): StateOrErrors = canCancel.map(_ => CanceledIssuedInvoice(id))

  override def canCancel: DomainEventOrErrors = Good(InvoiceCanceled(id))
}

case class CanceledIssuedInvoice(id: InvoiceId) extends AbstractInvoiceState(AlreadyCanceledError(id))

object Invoice {

  sealed abstract class InvoiceError(message: String) extends Error(message)

  case class ItemIdsNotFoundError(id: InvoiceId, itemIds: Seq[ItemId])
    extends InvoiceError(s"Invoice with id = $id does not contain one of these item ids (${itemIds.mkString(", ")})")

  case class AlreadyCanceledError(id: InvoiceId)
    extends InvoiceError(s"Invoice with id = $id does is already canceled")

  case class AlreadyIssuedError(id: InvoiceId)
    extends InvoiceError(s"Invoice with id = $id does is already issued")

  case class EmptyInvoiceError(id: InvoiceId)
    extends InvoiceError(s"Invoice with id = $id does is empty")

  case object UninitializedInvoiceError extends InvoiceError("Invoice is not initialized yet")

  case object AlreadyInitializedError extends InvoiceError("Invoice is already initialized")

  object Protocol {
    sealed trait InvoiceDomainEvent extends DomainEvent
    case class InvoiceCreated(id: InvoiceId, client: Client) extends InvoiceDomainEvent
    case class ItemsAdded(id: InvoiceId, lineItems: Seq[LineItem]) extends InvoiceDomainEvent
    case class ItemsDeleted(id: InvoiceId, itemIds: Seq[ItemId]) extends InvoiceDomainEvent
    case class InvoiceCanceled(id: InvoiceId) extends InvoiceDomainEvent
    case class InvoiceIssued(id: InvoiceId) extends InvoiceDomainEvent
  }

}
