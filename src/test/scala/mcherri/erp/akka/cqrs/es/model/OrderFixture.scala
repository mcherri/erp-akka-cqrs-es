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

import mcherri.erp.akka.cqrs.es.model
import org.scalactic.{Every, Or}

trait OrderFixture extends CommonFixture {
  protected val id: Or[OrderId, Every[OrderId.OrderIdError]] = OrderId(UUID.randomUUID())
  protected val client: Or[Client, Every[Error]] = PersonId(1).flatMap(id => model.Client(id))

  protected val emptyOrder: Or[OrderState, Every[Error]] = for (
    id1 <- id;
    client1 <- client;
    state <- UninitializedOrder$.init(id1, client1)
  ) yield state

  protected val order: Or[OrderState, Every[Error]] = for (
    state1 <- emptyOrder;
    lineItemSeq1 <- lineItemSeq;
    state2 <- state1.add(lineItemSeq1)
  ) yield state2

  protected val canceledOrder: Or[OrderState, Every[Error]] = for (
    state1 <- order;
    state2 <- state1.cancel()
  ) yield state2

  protected val issuedOrder: Or[OrderState, Every[Error]] = for (
    state1 <- order;
    state2 <- state1.issue()
  ) yield state2
}

