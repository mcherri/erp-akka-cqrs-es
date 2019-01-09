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

import mcherri.erp.akka.cqrs.es.model.PersonId.PersonIdError
import mcherri.erp.akka.cqrs.es.model.User.{AlreadyDisabledError, UserError}
import org.scalactic._

abstract case class PersonId private[PersonId](value: Long) {
  def copy(value: Long = value): PersonId Or Every[PersonIdError] = PersonId.apply(value)
}

object PersonId {
  sealed abstract class PersonIdError(message: String) extends Error(message)
  case class IdError(value: Long)
    extends PersonIdError(s"PersonId with value= $value that is not positive")

  def apply(value: Long): PersonId Or Every[PersonIdError] = {
    if (value > 0) {
      Good(new PersonId(value) {})
    } else {
      Bad(One(IdError(value)))
    }
  }
}

trait Person {
  def id: PersonId

  /*def name: String*/

}

trait Corporation {
  def corporationName: String

  def corporationAddress: Address
}

abstract case class User private[User](id: PersonId /*, name: String, username: String*/, disabled: Boolean = false) extends Person {
  def copy(id: PersonId = id, disabled: Boolean = disabled) = User.apply(id, disabled)

  def disable(): User Or Every[UserError] = {
    if (disabled) {
      Bad(One(AlreadyDisabledError(id)))
    } else {
      copy(disabled = true)
    }
  }
}

object User {
  sealed abstract class UserError(message: String) extends Error(message)
  case class AlreadyDisabledError(id: PersonId) extends UserError(s"User with id = $id is already disabled")

  //noinspection RedundantDefaultArgument
  def apply(id: PersonId): User Or Every[UserError] = User.apply(id, disabled = false)

  def apply(id: PersonId /*, name: String, username: String*/ , disabled: Boolean = false): User Or Every[UserError] =
    Good(new User(id, disabled) {})
}


case class Client(id: PersonId/*, name: String,
                  corporationName: String, corporationAddress: Address*/) extends Person /*with Corporation*/

case class Supplier(id: PersonId /*, name: String,
                    corporationName: String, corporationAddress: Address*/) extends Person /*with Corporation*/