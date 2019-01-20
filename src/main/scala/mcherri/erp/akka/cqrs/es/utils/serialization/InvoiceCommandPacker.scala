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
import mcherri.erp.akka.cqrs.es.aggregate.InvoiceAggregate
import mcherri.erp.akka.cqrs.es.model.{Client, PersonId, protobuf}
import mcherri.erp.akka.cqrs.es.utils.serialization.InvoiceCommandPacker._
import scalapb.{GeneratedMessage, Message}

class InvoiceCommandPacker extends InvoicePacker {
  override def unpack: PartialFunction[any.Any, AnyRef] = {
    case any@com.google.protobuf.any.Any(`CreateInvoiceManifest`, _) =>
      val protobufCreateInvoice = any.unpack[protobuf.invoice.CreateInvoice]
      val createInvoice = for (
        personId <- PersonId(protobufCreateInvoice.client.id.toLong);
        client <- Client(personId)
      ) yield InvoiceAggregate.Protocol.CreateInvoice(client)
      createInvoice.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`AddItemsManifest`, _) =>
      val protobufAddItems = any.unpack[protobuf.invoice.AddItems]
      val addItems = for (
        lineItems <- toLineItems(protobufAddItems.lineItems)
      ) yield InvoiceAggregate.Protocol.AddItems(lineItems)
      addItems.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`DeleteItemsManifest`, _) =>
      val protobufDeleteItems = any.unpack[protobuf.invoice.DeleteItems]
      val deleteItems = for (
        itemIds <- toItemIds(protobufDeleteItems.itemIds)
      ) yield InvoiceAggregate.Protocol.DeleteItems(itemIds)
      deleteItems.getOrElse(throw new NotSerializableException())
    case com.google.protobuf.any.Any(`CancelInvoiceManifest`, _) =>
      InvoiceAggregate.Protocol.CancelInvoice()
    case com.google.protobuf.any.Any(`IssueInvoiceManifest`, _) =>
      InvoiceAggregate.Protocol.IssueInvoice()
  }

  override def pack: PartialFunction[AnyRef, GeneratedMessage with Message[_]] = {
    case InvoiceAggregate.Protocol.CreateInvoice(Client(personId)) =>
      protobuf.invoice.CreateInvoice(protobuf.person.Client(personId.value.toString))
    case InvoiceAggregate.Protocol.AddItems(lineItems) =>
      protobuf.invoice.AddItems(toProtobufLineItems(lineItems))
    case InvoiceAggregate.Protocol.DeleteItems(itemIds) =>
      val protobufItemIds = itemIds.map(_.value.toString)
      protobuf.invoice.DeleteItems(protobufItemIds)
    case InvoiceAggregate.Protocol.CancelInvoice() =>
      protobuf.invoice.CancelInvoice()
    case InvoiceAggregate.Protocol.IssueInvoice() =>
      protobuf.invoice.IssueInvoice()
  }
}

object InvoiceCommandPacker {
  val CreateInvoiceManifest: String = Packer.manifest(protobuf.invoice.CreateInvoice)
  val AddItemsManifest: String = Packer.manifest(protobuf.invoice.AddItems)
  val DeleteItemsManifest: String = Packer.manifest(protobuf.invoice.DeleteItems)
  val CancelInvoiceManifest: String = Packer.manifest(protobuf.invoice.CancelInvoice)
  val IssueInvoiceManifest: String = Packer.manifest(protobuf.invoice.IssueInvoice)

  def apply(): InvoiceCommandPacker = new InvoiceCommandPacker()
}
