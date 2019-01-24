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

import com.google.protobuf.any
import mcherri.erp.akka.cqrs.es.aggregate.Command
import mcherri.erp.akka.cqrs.es.model
import mcherri.erp.akka.cqrs.es.model.{AggregateId, DomainEvent, LongAggregateId, State, protobuf}
import mcherri.erp.akka.cqrs.es.utils.serialization.DummiesPacker._
import scalapb.{GeneratedMessage, Message}

class DummiesPacker extends Packer {
  override def unpack: PartialFunction[any.Any, AnyRef] = {
    case com.google.protobuf.any.Any(`DummyEventManifest`, _) => DummyEvent
    case com.google.protobuf.any.Any(`DummyValidCommandManifest`, _) => DummyValidCommand
    case com.google.protobuf.any.Any(`DummyInvalidCommandManifest`, _) => DummyInvalidCommand
    case com.google.protobuf.any.Any(`DummyErrorManifest`, _) => DummyError
    case com.google.protobuf.any.Any(`DummyStateManifest`, _) => DummyState
  }

  override def pack: PartialFunction[AnyRef, GeneratedMessage with Message[_]] = {
    case DummyEvent =>
      protobuf.dummy.DummyEvent()
    case DummyValidCommand =>
      protobuf.dummy.DummyValidCommand()
    case DummyInvalidCommand =>
      protobuf.dummy.DummyInvalidCommand()
    case DummyError =>
      protobuf.dummy.DummyError()
    case DummyState =>
      protobuf.dummy.DummyState()
  }
}

object DummiesPacker {
  val DummyValidCommandManifest: String = Packer.manifest(protobuf.dummy.DummyValidCommand)
  val DummyInvalidCommandManifest: String = Packer.manifest(protobuf.dummy.DummyInvalidCommand)
  val DummyEventManifest: String = Packer.manifest(protobuf.dummy.DummyEvent)
  val DummyErrorManifest: String = Packer.manifest(protobuf.dummy.DummyError)
  val DummyStateManifest: String = Packer.manifest(protobuf.dummy.DummyState)

  def apply(): DummiesPacker = new DummiesPacker()

  // Dummies for testing
  case object DummyValidCommand extends Command

  case object DummyInvalidCommand extends Command

  case object DummyEvent extends DomainEvent {
    override def id: AggregateId = new LongAggregateId {
      override def id: Long = 1
    }
  }

  case object DummyError extends model.Error("An error")

  case object DummyState extends State

}