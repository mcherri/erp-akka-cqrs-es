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
import mcherri.erp.akka.cqrs.es.model.protobuf
import mcherri.erp.akka.cqrs.es.utils.serialization.ScalaticPacker._
import org.scalactic.{Bad, Good, One}
import scalapb.{GeneratedMessage, Message}

class ScalaticPacker(parent: Packer) extends Packer {
  override def unpack: PartialFunction[any.Any, AnyRef] = {
    case any@com.google.protobuf.any.Any(`GoodManifest`, _) =>
      val protobufGood = any.unpack[protobuf.scalactic.Good]
      Good(parent.unpack(protobufGood.g))
    case any@com.google.protobuf.any.Any(`BadManifest`, _) =>
      val protobufBad = any.unpack[protobuf.scalactic.Bad]
      Bad(parent.unpack(protobufBad.b))
    case any@com.google.protobuf.any.Any(`OneManifest`, _) =>
      val protobufOne = any.unpack[protobuf.scalactic.One]
      One(parent.unpack(protobufOne.loneElement))
  }

  override def pack: PartialFunction[AnyRef, GeneratedMessage with Message[_]] = {
    case Good(anyRef: AnyRef) =>
      val message = parent.pack(anyRef)
      protobuf.scalactic.Good(Packer.wrap(message))
    case Bad(anyRef: AnyRef) =>
      val message = parent.pack(anyRef)
      protobuf.scalactic.Bad(Packer.wrap(message))
    case One(anyRef: AnyRef) =>
      val message = parent.pack(anyRef)
      protobuf.scalactic.One(Packer.wrap(message))
  }
}

object ScalaticPacker {
  val GoodManifest: String = Packer.manifest(protobuf.scalactic.Good)
  val BadManifest: String = Packer.manifest(protobuf.scalactic.Bad)
  val OneManifest: String = Packer.manifest(protobuf.scalactic.One)

  def apply(parent: Packer): ScalaticPacker = new ScalaticPacker(parent: Packer)
}