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
import mcherri.erp.akka.cqrs.es.aggregate.OrderAggregate
import mcherri.erp.akka.cqrs.es.model.{Client, OrderId, PersonId, protobuf}
import mcherri.erp.akka.cqrs.es.utils.serialization.OrderCommandPacker._
import scalapb.{GeneratedMessage, Message}

class OrderCommandPacker extends OrderPacker {
  override def unpack: PartialFunction[any.Any, AnyRef] = {
    case any@com.google.protobuf.any.Any(`CreateOrderManifest`, _) =>
      val protobufCreateOrder = any.unpack[protobuf.order.CreateOrder]
      val createOrder = for (
        personId <- PersonId(protobufCreateOrder.client.id.toLong);
        client <- Client(personId)
      ) yield OrderAggregate.Protocol.CreateOrder(client)
      createOrder.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`AddItemsManifest`, _) =>
      val protobufAddItems = any.unpack[protobuf.order.AddItems]
      val addItems = for (
        id <- OrderId(UUID.fromString(protobufAddItems.id));
        lineItems <- toLineItems(protobufAddItems.lineItems)
      ) yield OrderAggregate.Protocol.AddItems(id, lineItems)
      addItems.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`DeleteItemsManifest`, _) =>
      val protobufDeleteItems = any.unpack[protobuf.order.DeleteItems]
      val deleteItems = for (
        id <- OrderId(UUID.fromString(protobufDeleteItems.id));
        itemIds <- toItemIds(protobufDeleteItems.itemIds)
      ) yield OrderAggregate.Protocol.DeleteItems(id, itemIds)
      deleteItems.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`CancelOrderManifest`, _) =>
      val protobufCancelOrder = any.unpack[protobuf.order.CancelOrder]
      val cancelOrder = for (
        id <- OrderId(UUID.fromString(protobufCancelOrder.id))
      ) yield OrderAggregate.Protocol.CancelOrder(id)
      cancelOrder.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`IssueOrderManifest`, _) =>
      val protobufIssueOrder = any.unpack[protobuf.order.IssueOrder]
      val issueOrder = for (
        id <- OrderId(UUID.fromString(protobufIssueOrder.id))
      ) yield OrderAggregate.Protocol.IssueOrder(id)
      issueOrder.getOrElse(throw new NotSerializableException())
  }

  override def pack: PartialFunction[AnyRef, GeneratedMessage with Message[_]] = {
    case OrderAggregate.Protocol.CreateOrder(Client(personId)) =>
      protobuf.order.CreateOrder(protobuf.person.Client(personId.value))
    case OrderAggregate.Protocol.AddItems(id, lineItems) =>
      protobuf.order.AddItems(id.value, toProtobufLineItems(lineItems))
    case OrderAggregate.Protocol.DeleteItems(id, itemIds) =>
      val protobufItemIds = itemIds.map(_.value)
      protobuf.order.DeleteItems(id.value, protobufItemIds)
    case OrderAggregate.Protocol.CancelOrder(id) =>
      protobuf.order.CancelOrder(id.value)
    case OrderAggregate.Protocol.IssueOrder(id) =>
      protobuf.order.IssueOrder(id.value)
  }
}

object OrderCommandPacker {
  val CreateOrderManifest: String = Packer.manifest(protobuf.order.CreateOrder)
  val AddItemsManifest: String = Packer.manifest(protobuf.order.AddItems)
  val DeleteItemsManifest: String = Packer.manifest(protobuf.order.DeleteItems)
  val CancelOrderManifest: String = Packer.manifest(protobuf.order.CancelOrder)
  val IssueOrderManifest: String = Packer.manifest(protobuf.order.IssueOrder)

  def apply(): OrderCommandPacker = new OrderCommandPacker()
}
