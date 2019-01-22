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
import mcherri.erp.akka.cqrs.es.model._
import mcherri.erp.akka.cqrs.es.utils.serialization.OrderStatePacker._
import scalapb.{GeneratedMessage, Message}

class OrderStatePacker extends OrderPacker {
  override def unpack: PartialFunction[any.Any, AnyRef] = {
    case com.google.protobuf.any.Any(`UninitializedOrderManifest`, _) =>
      UninitializedOrder$
    case any@com.google.protobuf.any.Any(`EmptyOrderManifest`, _) =>
      val protobufEmptyOrder = any.unpack[protobuf.order.EmptyOrder]
      val protobufClient = protobufEmptyOrder.client
      val emptyOrder = for (
        orderId <- OrderId(UUID.fromString(protobufEmptyOrder.id));
        personId <- PersonId(protobufClient.id.toLong);
        client <- Client(personId)
      ) yield EmptyOrder(orderId, client)
      emptyOrder.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`DraftOrderManifest`, _) =>
      val protobufDraftOrder = any.unpack[protobuf.order.DraftOrder]
      val protobufClient = protobufDraftOrder.client
      val draftOrder = for (
        orderId <- OrderId(UUID.fromString(protobufDraftOrder.id));
        personId <- PersonId(protobufClient.id.toLong);
        client <- Client(personId);
        lineItems <- toLineItems(protobufDraftOrder.lineItems)
      ) yield DraftOrder(orderId, client, lineItems)
      draftOrder.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`CanceledOrderManifest`, _) =>
      val protobufCanceledOrder = any.unpack[protobuf.order.CanceledOrder]
      val canceledOrder = for (
        orderId <- OrderId(UUID.fromString(protobufCanceledOrder.id))
      ) yield CanceledOrder(orderId)
      canceledOrder.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`IssuedOrderManifest`, _) =>
      val protobufIssuedOrder = any.unpack[protobuf.order.IssuedOrder]
      val issuedOrder = for (
        orderId <- OrderId(UUID.fromString(protobufIssuedOrder.id))
      ) yield IssuedOrder(orderId)
      issuedOrder.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`CanceledIssuedOrderManifest`, _) =>
      val protobufCanceledIssuedOrder = any.unpack[protobuf.order.CanceledIssuedOrder]
      val canceledIssuedOrder = for (
        orderId <- OrderId(UUID.fromString(protobufCanceledIssuedOrder.id))
      ) yield CanceledIssuedOrder(orderId)
      canceledIssuedOrder.getOrElse(throw new NotSerializableException())
  }

  override def pack: PartialFunction[AnyRef, GeneratedMessage with Message[_]] = {
    case UninitializedOrder$ =>
      protobuf.order.UninitializedOrder()
    case EmptyOrder(id, Client(personId)) =>
      protobuf.order.EmptyOrder(id.value.toString,
        protobuf.person.Client(personId.value.toString))
    case DraftOrder(id, Client(personId), itemLines) =>
      protobuf.order.DraftOrder(id.value.toString,
        protobuf.person.Client(personId.value.toString),
        toProtobufLineItems(itemLines))
    case CanceledOrder(id) =>
      protobuf.order.CanceledOrder(id.value.toString)
    case IssuedOrder(id) =>
      protobuf.order.IssuedOrder(id.value.toString)
    case CanceledIssuedOrder(id) =>
      protobuf.order.CanceledIssuedOrder(id.value.toString)
  }
}

object OrderStatePacker {
  val UninitializedOrderManifest: String = Packer.manifest(protobuf.order.UninitializedOrder)
  val EmptyOrderManifest: String = Packer.manifest(protobuf.order.EmptyOrder)
  val DraftOrderManifest: String = Packer.manifest(protobuf.order.DraftOrder)
  val CanceledOrderManifest: String = Packer.manifest(protobuf.order.CanceledOrder)
  val IssuedOrderManifest: String = Packer.manifest(protobuf.order.IssuedOrder)
  val CanceledIssuedOrderManifest: String = Packer.manifest(protobuf.order.CanceledIssuedOrder)

  def apply(): OrderStatePacker = new OrderStatePacker()
}
