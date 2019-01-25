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

import mcherri.erp.akka.cqrs.es.model.Client.ClientError
import mcherri.erp.akka.cqrs.es.model.PersonId.PersonIdError
import mcherri.erp.akka.cqrs.es.model.Supplier.SupplierError
import mcherri.erp.akka.cqrs.es.model.User.Protocol.{UserCreated, UserDisabled, UserDomainEvent}
import mcherri.erp.akka.cqrs.es.model.User.{AlreadyDisabledError, AlreadyInitializedError, UserError}
import org.scalactic._

import scala.collection.immutable.Seq

abstract case class PersonId private[PersonId](id: Long) extends LongAggregateId {
  def copy(id: Long = id): PersonId Or Every[PersonIdError] = PersonId.apply(id)
}

object PersonId {

  def apply(id: Long): PersonId Or Every[PersonIdError] = {
    if (id > 0) {
      Good(new PersonId(id) {})
    } else {
      Bad(One(IdError(id)))
    }
  }

  sealed abstract class PersonIdError(message: String) extends Error(message)

  case class IdError(id: Long)
    extends PersonIdError(s"PersonId with value = $id that is not positive")

}

sealed trait Person {
  def id: PersonId

  /*def name: String*/

}

trait Corporation {
  def corporationName: String

  def corporationAddress: Address
}

/*
 * TODO: UserState models the users states as a FSM. This may be over-engineering.
 * User operations can be modeled as normal CRUD operations an no need for CQRS/ES
 * for them. I will consider this one the read side is implemented
 */
trait UserState extends State {
  override type StateOrErrors = UserState Or Every[Error]

  override type DomainEvent = UserDomainEvent

  def canInit(id: PersonId): DomainEventsOrErrors

  def init(id: PersonId): StateOrErrors

  def canDisable: DomainEventsOrErrors

  def disable(): StateOrErrors
}

abstract class AbstractUserState(defaultErrors: Every[UserError]) extends UserState {
  override def canInit(id: PersonId): DomainEventsOrErrors = Bad(defaultErrors)

  override def init(id: PersonId): StateOrErrors = Bad(defaultErrors)

  override def canDisable: DomainEventsOrErrors = Bad(defaultErrors)

  override def disable(): StateOrErrors = Bad(defaultErrors)
}

case object UninitializedUser extends AbstractUserState(One(User.UninitializedError)) {
  override def init(id: PersonId): StateOrErrors =
    canInit(id).map(_ => ActiveUser(id))

  override def canInit(id: PersonId): DomainEventsOrErrors = {
    Good(Seq(UserCreated(id)))
  }
}

case class ActiveUser(id: PersonId) extends AbstractUserState(One(AlreadyInitializedError)) {
  override def disable(): StateOrErrors =
    canDisable.map(_ => DisabledUser(id))

  override def canDisable: DomainEventsOrErrors = Good(Seq(UserDisabled(id)))
}

case class DisabledUser(id: PersonId) extends AbstractUserState(One(AlreadyInitializedError)) {
  override def canDisable: DomainEventsOrErrors = Bad(One(AlreadyDisabledError(id)))

  override def disable(): StateOrErrors = Bad(One(AlreadyDisabledError(id)))
}

abstract case class User private[User](id: PersonId)

object User {

  sealed abstract class UserError(message: String) extends Error(message)

  case class AlreadyDisabledError(id: PersonId) extends UserError(s"User with id = $id is already disabled")

  case object UninitializedError extends UserError("User is not initialized yet")

  case object AlreadyInitializedError extends UserError("User is already initialized")

  object Protocol {

    sealed trait UserDomainEvent extends DomainEvent

    case class UserCreated(id: PersonId) extends UserDomainEvent

    case class UserDisabled(id: PersonId) extends UserDomainEvent

  }

}

abstract case class Client private[Client](id: PersonId) {
  def copy(id: PersonId = id): Client Or Every[ClientError] = Client.apply(id)
}

object Client {

  def apply(id: PersonId): Client Or Every[ClientError] = Good(new Client(id) {})

  sealed abstract class ClientError(message: String) extends Error(message)

}

abstract case class Supplier private[Supplier](id: PersonId) {
  def copy(id: PersonId = id): Supplier Or Every[SupplierError] = Supplier.apply(id)
}

object Supplier {

  def apply(id: PersonId): Supplier Or Every[SupplierError] = Good(new Supplier(id) {})

  sealed abstract class SupplierError(message: String) extends Error(message)

}
