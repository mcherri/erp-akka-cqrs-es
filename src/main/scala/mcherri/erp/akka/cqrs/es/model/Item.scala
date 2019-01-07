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

import org.scalactic._
import org.sisioh.baseunits.scala.money.Money

sealed abstract class ItemIdError(message: String) extends Error(message)

case class ItemUuidError(value: UUID)
  extends ItemIdError(s"ItemId with UUID = $value is not version 4 UUID")

abstract case class ItemId private[ItemId](value: UUID) {
  def copy(value: UUID = value): ItemId Or Every[ItemIdError] = ItemId.apply(value)
}

object ItemId {
  def apply(value: UUID): ItemId Or Every[ItemIdError] = {
    if (value.version() == 4) {
      Good(new ItemId(value) {})
    } else {
      Bad(One(ItemUuidError(value)))
    }
  }
}

case class Item(id: ItemId, code: String, description: String, price: Money, remainingQuantity: BigDecimal)

/*extends Stamp*/
