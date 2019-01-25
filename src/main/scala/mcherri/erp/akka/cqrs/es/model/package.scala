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
package mcherri.erp.akka.cqrs.es

import mcherri.erp.akka.cqrs.es.model.protobuf.common
import org.scalactic.{Every, Or}
import org.sisioh.baseunits.scala.money

import scala.collection.immutable.Seq

package object model {

  /*
   * TODO: DomainEvent should have metadata like:
   * - occurredAt: TimePoint
   * - occurredBy: User
   *
   * This will help the read side to track thing like:
   * - createdBy: User
   * - createdAt: TimePoint
   * - updatedBy: User
   * - updatedAt: TimePoint
   */
  trait DomainEvent {
    def id: AggregateId
  }

  trait State {
    type StateOrErrors
    type DomainEvent
    final type DomainEventsOrErrors = Seq[DomainEvent] Or Every[Error]
  }

  // TODO: Move this to protobuf package somehow
  implicit class RichMoney(val m: money.Money) extends AnyVal {
    def toProtobuf: common.Money = common.Money(m.amount.toString(), m.currency.getCurrencyCode)
  }

}
