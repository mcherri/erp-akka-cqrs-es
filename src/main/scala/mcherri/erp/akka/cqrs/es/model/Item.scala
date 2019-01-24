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

import mcherri.erp.akka.cqrs.es.model.ItemId.ItemIdError
import org.scalactic._
import org.sisioh.baseunits.scala.money.Money

abstract case class ItemId private[ItemId](id: UUID) extends UuidAggregateId {
  def copy(id: UUID = id): ItemId Or Every[ItemIdError] = ItemId.apply(id)
}

object ItemId {

  def apply(id: UUID): ItemId Or Every[ItemIdError] = {
    if (id.version() == 4) {
      Good(new ItemId(id) {})
    } else {
      Bad(One(UuidError(id)))
    }
  }

  sealed abstract class ItemIdError(message: String) extends Error(message)

  case class UuidError(id: UUID)
    extends ItemIdError(s"ItemId with UUID = $id is not version 4 UUID")
}

abstract case class Item private[Item](id: ItemId, code: String, description: String,
                                       price: Money, remainingQuantity: BigDecimal) {
  def copy(id: ItemId = id, code: String = code, description: String = description,
           price: Money = price, remainingQuantity: BigDecimal = remainingQuantity): Item Or Every[Error] =
    Item.apply(id, code, description, price, remainingQuantity)
}

object Item {
  def apply(id: ItemId, code: String, description: String,
            price: Money, remainingQuantity: BigDecimal): Item Or Every[Error] = {
    Good(new Item(id, code, description, price, remainingQuantity) {})
  }
}