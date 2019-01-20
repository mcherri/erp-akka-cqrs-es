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

import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

trait Packer {
  def unpack: PartialFunction[com.google.protobuf.any.Any, AnyRef]

  def pack: PartialFunction[AnyRef, GeneratedMessage with Message[_]]
}

object Packer {
  val Prefix = "type.googleapis.com/"

  def wrap[A <: GeneratedMessage](generatedMessage: A): com.google.protobuf.any.Any =
    com.google.protobuf.any.Any(
      typeUrl = manifest(generatedMessage.companion),
      value = generatedMessage.toByteString
    )

  def manifest(companion: GeneratedMessageCompanion[_]): String = Prefix + companion.scalaDescriptor.fullName

}