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
import java.util.UUID

import com.google.protobuf.any
import mcherri.erp.akka.cqrs.es.model.{Order, OrderId, protobuf}
import mcherri.erp.akka.cqrs.es.utils.serialization.OrderErrorPacker._
import scalapb.{GeneratedMessage, Message}

class OrderErrorPacker extends OrderPacker {
  override def unpack: PartialFunction[any.Any, AnyRef] = {
    case any@com.google.protobuf.any.Any(`ItemIdsNotFoundErrorManifest`, _) =>
      val protobufError = any.unpack[protobuf.order.ItemIdsNotFoundError]
      val itemIdsNotFoundError = for (
        orderId <- OrderId(UUID.fromString(protobufError.id));
        itemIds <- toItemIds(protobufError.itemIds)
      ) yield Order.ItemIdsNotFoundError(orderId, itemIds)
      itemIdsNotFoundError.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`AlreadyCanceledErrorManifest`, _) =>
      val protobufError = any.unpack[protobuf.order.AlreadyCanceledError]
      val alreadyCanceledError = for (
        orderId <- OrderId(UUID.fromString(protobufError.id))
      ) yield Order.AlreadyCanceledError(orderId)
      alreadyCanceledError.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`AlreadyIssuedErrorManifest`, _) =>
      val protobufError = any.unpack[protobuf.order.AlreadyIssuedError]
      val alreadyIssuedError = for (
        orderId <- OrderId(UUID.fromString(protobufError.id))
      ) yield Order.AlreadyIssuedError(orderId)
      alreadyIssuedError.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`EmptyOrderErrorManifest`, _) =>
      val protobufError = any.unpack[protobuf.order.EmptyOrderError]
      val emptyOrderError = for (
        orderId <- OrderId(UUID.fromString(protobufError.id))
      ) yield Order.EmptyOrderError(orderId)
      emptyOrderError.getOrElse(throw new NotSerializableException())
    case com.google.protobuf.any.Any(`UninitializedOrderErrorManifest`, _) =>
      Order.UninitializedOrderError$
    case com.google.protobuf.any.Any(`AlreadyInitializedErrorManifest`, _) =>
      Order.AlreadyInitializedError
  }

  override def pack: PartialFunction[AnyRef, GeneratedMessage with Message[_]] = {
    case Order.ItemIdsNotFoundError(id, itemIds) =>
      val protobufItemIds = itemIds.map(_.value.toString)
      protobuf.order.ItemIdsNotFoundError(id.value.toString, protobufItemIds)
    case Order.AlreadyCanceledError(id) =>
      protobuf.order.AlreadyCanceledError(id.value.toString)
    case Order.AlreadyIssuedError(id) =>
      protobuf.order.AlreadyIssuedError(id.value.toString)
    case Order.EmptyOrderError(id) =>
      protobuf.order.EmptyOrderError(id.value.toString)
    case Order.UninitializedOrderError$ =>
      protobuf.order.UninitializedOrderError()
    case Order.AlreadyInitializedError =>
      protobuf.order.AlreadyInitializedError()
  }
}

object OrderErrorPacker {
  val ItemIdsNotFoundErrorManifest: String = Packer.manifest(protobuf.order.ItemIdsNotFoundError)
  val AlreadyCanceledErrorManifest: String = Packer.manifest(protobuf.order.AlreadyCanceledError)
  val AlreadyIssuedErrorManifest: String = Packer.manifest(protobuf.order.AlreadyIssuedError)
  val EmptyOrderErrorManifest: String = Packer.manifest(protobuf.order.EmptyOrderError)
  val UninitializedOrderErrorManifest: String = Packer.manifest(protobuf.order.UninitializedOrderError)
  val AlreadyInitializedErrorManifest: String = Packer.manifest(protobuf.order.AlreadyInitializedError)

  def apply(): OrderErrorPacker = new OrderErrorPacker()
}