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

import akka.actor.ActorLogging
import akka.persistence.PersistentActor
import mcherri.erp.akka.cqrs.es.aggregate.Aggregate.Protocol.GetState
import mcherri.erp.akka.cqrs.es.model
import mcherri.erp.akka.cqrs.es.model.{DomainEvent, State, UninitializedError}
import org.scalactic._

abstract class Aggregate[T <: State] extends PersistentActor with ActorLogging {
  type StateOrError = T Or Every[model.Error]
  protected var state: StateOrError = Bad(One(UninitializedError(id)))

  override def receiveRecover: Receive = {
    case event: DomainEvent =>
      applyState(event)
  }

  override def receiveCommand: Receive = {
    case command: Command =>
      applyCommand(command).fold({
        persistAll[DomainEvent](_) { event =>
          applyState(event)
          sender() ! (Good(event): DomainEvent Or Every[model.Error])
          // TODO: Save snapshot
        }
      }, { errors =>
        sender() ! (Bad(errors): DomainEvent Or Every[model.Error])
      })
    case GetState =>
      sender() ! state
  }

  private def applyState(event: DomainEvent): Unit = {
    state = applyEvent(event)
    state.swap.foreach { errors: Every[model.Error] =>
      throw new IllegalStateException(errors.mkString("\n")) // Should not happen
    }
  }

  override def persistenceId: String = s"${getClass.getSimpleName}-$id"

  def id: String = self.path.name

  protected def applyCommand(command: Command): CommandResult

  protected def applyEvent(event: DomainEvent): StateOrError
}

object Aggregate {

  object Protocol {

    case object GetState // Mainly for testing
  }

}