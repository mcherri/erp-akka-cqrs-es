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

import java.io.NotSerializableException

import akka.serialization.Serializer
import com.google.protobuf.any
import scalapb.{GeneratedMessage, Message}

/*
 * TODO: I may or may not use https://github.com/bfil/scala-automapper. This class
 * will play an important role in schema evolution later.
 */
class ProtobufSerializer extends Serializer with Packer {
  // TODO: Load packers dynamically from application.conf
  private val packers = Seq(MessagePacker(), InvoiceCommandPacker(), InvoiceDomainEventPacker(),
    InvoiceStatePacker(), InvoiceErrorPacker(), ErrorsPacker(), ScalaticPacker(this), DummiesPacker())

  override def identifier: Int = 399697686

  override def includeManifest: Boolean = false

  override def fromBinary(bytes: Array[Byte], manifest: Option[Class[_]]): AnyRef =
    unpack(com.google.protobuf.any.Any.parseFrom(bytes))

  override def toBinary(o: AnyRef): Array[Byte] = Packer.wrap(pack(o)).toByteArray

  private def throwNotSerializableException: PartialFunction[com.google.protobuf.any.Any, AnyRef] = {
    case any: com.google.protobuf.any.Any => throw new NotSerializableException(any.typeUrl)
    case _ => throw new NotSerializableException()
  }

  override val unpack: PartialFunction[any.Any, AnyRef] =
    (packers.map(_.unpack) :+ throwNotSerializableException) reduce (_ orElse _)

  override val pack: PartialFunction[AnyRef, GeneratedMessage with Message[_]] =
    packers.map(_.pack) reduce (_ orElse _)
}
