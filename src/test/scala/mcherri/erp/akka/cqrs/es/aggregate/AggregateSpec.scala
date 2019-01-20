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

import akka.actor.{ActorRef, Props}
import mcherri.erp.akka.cqrs.es.aggregate.Aggregate.Protocol.GetState
import mcherri.erp.akka.cqrs.es.aggregate.RestartableActor.RestartActor
import mcherri.erp.akka.cqrs.es.utils.serialization.ProtobufSerializer._
import mcherri.erp.akka.cqrs.es.{ActorUnitSpec, model}
import org.scalactic.{Bad, Good, One}

import scala.collection.immutable.Seq

class AggregateSpec extends ActorUnitSpec {

  trait AggregateFixture {
    protected val aggregate: ActorRef = {
      system.actorOf(Props(new Aggregate[DummyState.type] with RestartableActor {
        override protected def applyCommand(command: Command): CommandResult = command match {
          case DummyValidCommand => Good(Seq(DummyEvent))
          case DummyInvalidCommand => Bad(One(DummyError))
        }

        override protected def applyEvent(event: model.DomainEvent): StateOrError = Good(DummyState)
      }))
    }
  }

  "An aggregate" should "reply with an event for accepted commands" in new AggregateFixture {
    aggregate ! DummyValidCommand
    expectMsgPF() {
      case Good(DummyEvent) => Unit
    }
  }

  it should "reply with an error for invalid commands" in new AggregateFixture {
    aggregate ! DummyInvalidCommand
    expectMsgPF() {
      case Bad(One(DummyError)) => Unit
    }
  }

  it should "persist it state after accepting commands" in new AggregateFixture {
    aggregate ! GetState
    expectMsgPF() {
      case Bad(_) => Unit // The initial state
    }

    aggregate ! DummyValidCommand
    expectMsgPF() {
      case Good(_) => Unit // State should be changed
    }

    // Now restart and get the state
    aggregate ! RestartActor
    aggregate ! GetState
    expectMsgPF() {
      case Good(_) => Unit // The state should be the good one that was persisted
    }
  }

}
