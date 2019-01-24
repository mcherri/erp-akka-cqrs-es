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

import com.google.protobuf.any
import mcherri.erp.akka.cqrs.es.aggregate.OrderAggregate
import mcherri.erp.akka.cqrs.es.model.{Client, PersonId, protobuf}
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
        lineItems <- toLineItems(protobufAddItems.lineItems)
      ) yield OrderAggregate.Protocol.AddItems(lineItems)
      addItems.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`DeleteItemsManifest`, _) =>
      val protobufDeleteItems = any.unpack[protobuf.order.DeleteItems]
      val deleteItems = for (
        itemIds <- toItemIds(protobufDeleteItems.itemIds)
      ) yield OrderAggregate.Protocol.DeleteItems(itemIds)
      deleteItems.getOrElse(throw new NotSerializableException())
    case com.google.protobuf.any.Any(`CancelOrderManifest`, _) =>
      OrderAggregate.Protocol.CancelOrder()
    case com.google.protobuf.any.Any(`IssueOrderManifest`, _) =>
      OrderAggregate.Protocol.IssueOrder()
  }

  override def pack: PartialFunction[AnyRef, GeneratedMessage with Message[_]] = {
    case OrderAggregate.Protocol.CreateOrder(Client(personId)) =>
      protobuf.order.CreateOrder(protobuf.person.Client(personId.value))
    case OrderAggregate.Protocol.AddItems(lineItems) =>
      protobuf.order.AddItems(toProtobufLineItems(lineItems))
    case OrderAggregate.Protocol.DeleteItems(itemIds) =>
      val protobufItemIds = itemIds.map(_.value)
      protobuf.order.DeleteItems(protobufItemIds)
    case OrderAggregate.Protocol.CancelOrder() =>
      protobuf.order.CancelOrder()
    case OrderAggregate.Protocol.IssueOrder() =>
      protobuf.order.IssueOrder()
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
