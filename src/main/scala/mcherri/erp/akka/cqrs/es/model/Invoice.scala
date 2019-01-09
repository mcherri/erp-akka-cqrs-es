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

import mcherri.erp.akka.cqrs.es.model.Invoice._
import mcherri.erp.akka.cqrs.es.utils.RichOr._
import org.scalactic.Accumulation._
import org.scalactic._
import org.sisioh.baseunits.scala.money.Money

sealed abstract class InvoiceIdError(message: String) extends Error(message)

case class InvoiceUuidError(value: UUID)
  extends InvoiceIdError(s"InvoiceId with UUID = $value is not version 4 UUID")

abstract case class InvoiceId private[InvoiceId](value: UUID) {
  def copy(value: UUID = value): InvoiceId Or Every[InvoiceIdError] = InvoiceId.apply(value)
}

object InvoiceId {
  def apply(value: UUID): InvoiceId Or Every[InvoiceIdError] = {
    if (value.version() == 4) {
      Good(new InvoiceId(value) {})
    } else {
      Bad(One(InvoiceUuidError(value)))
    }
  }
}

sealed abstract class LineItemError(message: String) extends Error(message)

case class NegativePriceError(price: Money)
  extends LineItemError(s"Line item with negative price $price")

case class NegativeOrZeroQuantityError(quantity: BigDecimal)
  extends LineItemError(s"Line item with negative or zero quantity $quantity")

case class MoreThanRemainingQuantityError(quantity: BigDecimal)
  extends LineItemError(s"Line item with quantity that is more than the item remaining quantity")

abstract case class LineItem private[LineItem](item: Item, price: Money, quantity: BigDecimal) {
  def copy(item: Item = item, price: Money = price, quantity: BigDecimal = quantity): LineItem Or Every[LineItemError] =
    LineItem.apply(item, price, quantity)
}

object LineItem {
  def apply(item: Item, price: Money, quantity: BigDecimal): LineItem Or Every[LineItemError] = {
    val priceOrError = if (price.isNegative) {
      Bad(One(NegativePriceError(price)))
    } else {
      Good(price)
    }
    val quantityOrError = quantity match {
      case q if q <= 0 => Bad(One(NegativeOrZeroQuantityError(quantity)))
      case q if q > item.remainingQuantity => Bad(One(MoreThanRemainingQuantityError(quantity)))
      case q => Good(q)
    }

    withGood(priceOrError, quantityOrError) {
      new LineItem(item, _, _) {}
    }
  }
}

abstract case class Invoice private[Invoice](id: InvoiceId, client: Client, itemLines: Seq[LineItem],
                                             canceled: Boolean = false, issued: Boolean = false) {
  def copy(id: InvoiceId = id, client: Client = client, itemLines: Seq[LineItem] = itemLines,
           canceled: Boolean = canceled, issued: Boolean = issued): Invoice Or Every[InvoiceError] =
    Invoice.apply(id, client, itemLines, canceled, issued)

  private def doIfValid(fn: => Invoice Or Every[Error]): Invoice Or Every[Error] = {
    (canceled, issued) match {
      case (true, true) => Bad(One(AlreadyCanceledError(id)) ++ One(AlreadyIssuedError(id)))
      case (true, _) => Bad(One(AlreadyCanceledError(id)))
      case (_, true) => Bad(One(AlreadyIssuedError(id)))
      case _ => fn
    }
  }

  def add(newLines: Seq[LineItem]): Invoice Or Every[Error] = {
    doIfValid {
      merge(newLines).flatMap(lines => copy(itemLines = lines))
    }
  }

  private def merge(newLines: Seq[LineItem]) = {
    (itemLines ++ newLines).groupBy(_.item.id).map {
      case (_, seq: Seq[LineItem]) =>
        seq.tail.foldLeft(Good(seq.head): LineItem Or Every[LineItemError]) { (acc, line1) =>
          acc.flatMap(line2 => line2.copy(quantity = line1.quantity + line2.quantity))
        }
    }.toSeq.sequence()
  }

  def delete(itemIds: Seq[ItemId]): Or[Invoice, Every[Error]] = {
    doIfValid {
      val size = itemLines.size
      val newItemLines = itemLines.filter(lineItem => !itemIds.contains(lineItem.item.id))

      if (newItemLines.size < size) {
        copy(itemLines = newItemLines)
      } else {
        // TODO: Identify which item was not found exactly
        Bad(One(ItemIdsNotFoundError(id, itemIds)))
      }
    }
  }

  def cancel(): Invoice Or Every[InvoiceError] = {
    if (canceled) {
      Bad(One(AlreadyCanceledError(id)))
    } else {
      copy(canceled = true)
    }
  }

  def issue(): Invoice Or Every[InvoiceError] = {
    (issued, itemLines.isEmpty) match {
      case (true, true) => Bad(One(AlreadyIssuedError(id)) ++ One(EmptyInvoiceError(id)))
      case (true, _) => Bad(One(AlreadyIssuedError(id)))
      case (_, true) => Bad(One(EmptyInvoiceError(id)))
      case _ => copy(issued = true)
    }
  }

  def count: Int = itemLines.size
}

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

  def apply(id: InvoiceId, client: Client): Invoice Or Every[InvoiceError] =
    apply(id, client, Seq.empty, canceled = false, issued = false)

  private[Invoice] def apply(id: InvoiceId, client: Client, itemLines: Seq[LineItem],
                             canceled: Boolean, issued: Boolean): Invoice Or Every[InvoiceError] =
    Good(new Invoice(id, client, itemLines, canceled, issued) {})

  /*
     createdBy: User, createdAt: TimePoint,
     updatedBy: User, updatedAt: TimePoint) extends Stamp
   */
}
