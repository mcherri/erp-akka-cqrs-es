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
import mcherri.erp.akka.cqrs.es.model.Order.Protocol
import mcherri.erp.akka.cqrs.es.model.{Client, OrderId, PersonId, protobuf}
import mcherri.erp.akka.cqrs.es.utils.serialization.OrderDomainEventPacker._
import scalapb.{GeneratedMessage, Message}

class OrderDomainEventPacker extends OrderPacker {
  override def unpack: PartialFunction[any.Any, AnyRef] = {
    case any@com.google.protobuf.any.Any(`OrderCreatedManifest`, _) =>
      val protobufOrderCreated = any.unpack[protobuf.order.OrderCreated]
      val protobufClient = protobufOrderCreated.client
      val orderCreated = for (
        orderId <- OrderId(UUID.fromString(protobufOrderCreated.id));
        personId <- PersonId(protobufClient.id.toLong);
        client <- Client(personId)
      ) yield Protocol.OrderCreated(orderId, client)
      orderCreated.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`ItemsAddedManifest`, _) =>
      val protobufItemsAdded = any.unpack[protobuf.order.ItemsAdded]
      val itemsAdded = for (
        orderId <- OrderId(UUID.fromString(protobufItemsAdded.id));
        lineItems <- toLineItems(protobufItemsAdded.lineItems)
      ) yield Protocol.ItemsAdded(orderId, lineItems)
      itemsAdded.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`ItemsDeletedManifest`, _) =>
      val protobufItemsDeleted = any.unpack[protobuf.order.ItemsDeleted]
      val itemsDeleted = for (
        orderId <- OrderId(UUID.fromString(protobufItemsDeleted.id));
        itemIds <- toItemIds(protobufItemsDeleted.itemIds)
      ) yield Protocol.ItemsDeleted(orderId, itemIds)
      itemsDeleted.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`OrderCanceledManifest`, _) =>
      val protobufOrderCanceled = any.unpack[protobuf.order.OrderCanceled]
      val orderCanceled = for (
        orderId <- OrderId(UUID.fromString(protobufOrderCanceled.id))
      ) yield Protocol.OrderCanceled(orderId)
      orderCanceled.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`OrderIssuedManifest`, _) =>
      val protobufOrderIssued = any.unpack[protobuf.order.OrderIssued]
      val orderIssued = for (
        orderId <- OrderId(UUID.fromString(protobufOrderIssued.id))
      ) yield Protocol.OrderCanceled(orderId)
      orderIssued.getOrElse(throw new NotSerializableException())
  }

  override def pack: PartialFunction[AnyRef, GeneratedMessage with Message[_]] = {
    case Protocol.OrderCreated(id, Client(personId)) =>
      protobuf.order.OrderCreated(id.value,
        protobuf.person.Client(personId.value))
    case Protocol.ItemsAdded(id, lineItems) =>
      protobuf.order.ItemsAdded(id.value, toProtobufLineItems(lineItems))
    case Protocol.ItemsDeleted(id, itemIds) =>
      val protobufItemIds = itemIds.map(_.value)
      protobuf.order.ItemsDeleted(id.value, protobufItemIds)
    case Protocol.OrderCanceled(id) =>
      protobuf.order.OrderCanceled(id.value)
    case Protocol.OrderIssued(id) =>
      protobuf.order.OrderIssued(id.value)
  }
}

object OrderDomainEventPacker {
  val OrderCreatedManifest: String = Packer.manifest(protobuf.order.OrderCreated)
  val ItemsAddedManifest: String = Packer.manifest(protobuf.order.ItemsAdded)
  val ItemsDeletedManifest: String = Packer.manifest(protobuf.order.ItemsDeleted)
  val OrderCanceledManifest: String = Packer.manifest(protobuf.order.OrderCanceled)
  val OrderIssuedManifest: String = Packer.manifest(protobuf.order.OrderIssued)

  def apply(): OrderDomainEventPacker = new OrderDomainEventPacker()
}
