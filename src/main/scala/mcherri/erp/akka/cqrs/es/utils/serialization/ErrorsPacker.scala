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
import mcherri.erp.akka.cqrs.es.model.{UninitializedError, protobuf}
import mcherri.erp.akka.cqrs.es.utils.serialization.ErrorsPacker._
import scalapb.{GeneratedMessage, Message}

class ErrorsPacker extends Packer {
  override def unpack: PartialFunction[any.Any, AnyRef] = {
    case any@com.google.protobuf.any.Any(`UninitializedErrorManifest`, _) =>
      val protobufError = any.unpack[protobuf.error.UninitializedError]
      UninitializedError(protobufError.id)
  }

  override def pack: PartialFunction[AnyRef, GeneratedMessage with Message[_]] = {
    case UninitializedError(id) =>
      protobuf.error.UninitializedError(id)
  }
}

object ErrorsPacker {
  val UninitializedErrorManifest: String = Packer.manifest(protobuf.error.UninitializedError)

  def apply(): ErrorsPacker = new ErrorsPacker()
}
