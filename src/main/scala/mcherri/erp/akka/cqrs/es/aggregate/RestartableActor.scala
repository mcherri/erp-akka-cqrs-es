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

import akka.persistence.PersistentActor
import mcherri.erp.akka.cqrs.es.aggregate.RestartableActor.{RestartActor, RestartActorException}

/*
 * RestartableActor is a fork from https://tudorzgureanu.com/akka-persistence-testing-persistent-actors/
 */
trait RestartableActor extends PersistentActor {

  abstract override def receiveCommand: PartialFunction[Any, Unit] = super.receiveCommand orElse {
    case RestartActor => throw RestartActorException
  }
}

object RestartableActor {
  case object RestartActor

  private object RestartActorException extends Exception
}