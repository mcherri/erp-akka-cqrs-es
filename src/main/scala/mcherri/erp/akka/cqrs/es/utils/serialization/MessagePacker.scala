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
import mcherri.erp.akka.cqrs.es.aggregate.{Aggregate, RestartableActor}
import mcherri.erp.akka.cqrs.es.model.protobuf
import mcherri.erp.akka.cqrs.es.utils.serialization.MessagePacker._
import scalapb.{GeneratedMessage, Message}

class MessagePacker extends Packer {
  override def unpack: PartialFunction[any.Any, AnyRef] = {
    case com.google.protobuf.any.Any(`GetStateManifest`, _) =>
      Aggregate.Protocol.GetState
    case com.google.protobuf.any.Any(`RestartActorManifest`, _) =>
      RestartableActor.RestartActor
  }

  override def pack: PartialFunction[AnyRef, GeneratedMessage with Message[_]] = {
    case Aggregate.Protocol.GetState =>
      protobuf.command.GetState()
    case RestartableActor.RestartActor =>
      protobuf.command.RestartActor()
  }
}

object MessagePacker {
  val GetStateManifest: String = Packer.manifest(protobuf.command.GetState)
  val RestartActorManifest: String = Packer.manifest(protobuf.command.RestartActor)

  def apply(): MessagePacker = new MessagePacker()
}
