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

import mcherri.erp.akka.cqrs.es.utils.RichOr._
import org.scalactic.{Every, Or}
import org.sisioh.baseunits.scala.money

import scala.collection.immutable.Seq

trait CommonFixture {
  protected val oneEur = money.Money(BigDecimal("1.00"), money.Money.EUR)

  protected val lineItemSeq: Or[Seq[LineItem], Every[Error]] = (1 to 10).map { i =>
    val price = oneEur.times(Math.random() * 100)
    val quantity = Math.random() * 100 + 1
    ItemId(UUID.randomUUID()).flatMap(id => LineItem(id, s"abc$i", price, quantity))
  }.sequence()

  protected val anotherLineItemSeq: Or[Seq[LineItem], Every[Error]] =
    lineItemSeq.flatMap(_ (4).copy(quantity = 10)).map(Seq(_))

  protected val itemIdsToDelete: Or[Seq[ItemId], Every[Error]] =
    lineItemSeq.map(l => Seq(l(5).itemId, l(8).itemId))
}
