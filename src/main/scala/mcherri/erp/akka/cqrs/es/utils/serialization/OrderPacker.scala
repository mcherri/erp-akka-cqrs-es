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
package mcherri.erp.akka.cqrs.es.utils.serialization

import java.util.{Currency, UUID}

import mcherri.erp.akka.cqrs.es.model._
import mcherri.erp.akka.cqrs.es.model.protobuf.order
import mcherri.erp.akka.cqrs.es.utils.RichOr._
import org.scalactic.{Every, Or}
import org.sisioh.baseunits.scala.money.Money

import scala.collection.immutable

trait OrderPacker extends Packer {
  protected def toItemIds(protobufIds: immutable.Seq[String]): Or[immutable.Seq[ItemId], Every[ItemId.ItemIdError]] = {
    protobufIds.map { itemId =>
      ItemId(UUID.fromString(itemId))
    }.sequence()
  }

  protected def toLineItems(lineItems: immutable.Seq[order.LineItem]): Or[immutable.Seq[LineItem], Every[Error]] = {
    lineItems.collect {
      case order.LineItem(itemId, code, Some(protobuf.common.Money(amount, currency)), quantity) =>
        ItemId(UUID.fromString(itemId)).flatMap { itemId =>
          LineItem(itemId, code, Money(BigDecimal(amount), Currency.getInstance(currency)), BigDecimal(quantity))
        }
    }.sequence()
  }

  protected def toProtobufLineItems(lineItems: immutable.Seq[LineItem]): immutable.Seq[order.LineItem] = {
    lineItems.map {
      case LineItem(ItemId(itemId), code, money, quantity) =>
        protobuf.order.LineItem(itemId.toString, code,
          Some(money.toProtobuf), quantity.toString())
    }
  }
}
