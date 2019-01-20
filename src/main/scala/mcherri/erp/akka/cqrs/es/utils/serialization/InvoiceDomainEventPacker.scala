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
import mcherri.erp.akka.cqrs.es.model.Invoice.Protocol
import mcherri.erp.akka.cqrs.es.model.{Client, InvoiceId, PersonId, protobuf}
import mcherri.erp.akka.cqrs.es.utils.serialization.InvoiceDomainEventPacker._
import scalapb.{GeneratedMessage, Message}

class InvoiceDomainEventPacker extends InvoicePacker {
  override def unpack: PartialFunction[any.Any, AnyRef] = {
    case any@com.google.protobuf.any.Any(`InvoiceCreatedManifest`, _) =>
      val protobufInvoiceCreated = any.unpack[protobuf.invoice.InvoiceCreated]
      val protobufClient = protobufInvoiceCreated.client
      val invoiceCreated = for (
        invoiceId <- InvoiceId(UUID.fromString(protobufInvoiceCreated.id));
        personId <- PersonId(protobufClient.id.toLong);
        client <- Client(personId)
      ) yield Protocol.InvoiceCreated(invoiceId, client)
      invoiceCreated.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`ItemsAddedManifest`, _) =>
      val protobufItemsAdded = any.unpack[protobuf.invoice.ItemsAdded]
      val itemsAdded = for (
        invoiceId <- InvoiceId(UUID.fromString(protobufItemsAdded.id));
        lineItems <- toLineItems(protobufItemsAdded.lineItems)
      ) yield Protocol.ItemsAdded(invoiceId, lineItems)
      itemsAdded.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`ItemsDeletedManifest`, _) =>
      val protobufItemsDeleted = any.unpack[protobuf.invoice.ItemsDeleted]
      val itemsDeleted = for (
        invoiceId <- InvoiceId(UUID.fromString(protobufItemsDeleted.id));
        itemIds <- toItemIds(protobufItemsDeleted.itemIds)
      ) yield Protocol.ItemsDeleted(invoiceId, itemIds)
      itemsDeleted.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`InvoiceCanceledManifest`, _) =>
      val protobufInvoiceCanceled = any.unpack[protobuf.invoice.InvoiceCanceled]
      val invoiceCanceled = for (
        invoiceId <- InvoiceId(UUID.fromString(protobufInvoiceCanceled.id))
      ) yield Protocol.InvoiceCanceled(invoiceId)
      invoiceCanceled.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`InvoiceIssuedManifest`, _) =>
      val protobufInvoiceIssued = any.unpack[protobuf.invoice.InvoiceIssued]
      val invoiceIssued = for (
        invoiceId <- InvoiceId(UUID.fromString(protobufInvoiceIssued.id))
      ) yield Protocol.InvoiceCanceled(invoiceId)
      invoiceIssued.getOrElse(throw new NotSerializableException())
  }

  override def pack: PartialFunction[AnyRef, GeneratedMessage with Message[_]] = {
    case Protocol.InvoiceCreated(id, Client(personId)) =>
      protobuf.invoice.InvoiceCreated(id.value.toString,
        protobuf.person.Client(personId.value.toString))
    case Protocol.ItemsAdded(id, lineItems) =>
      protobuf.invoice.ItemsAdded(id.value.toString, toProtobufLineItems(lineItems))
    case Protocol.ItemsDeleted(id, itemIds) =>
      val protobufItemIds = itemIds.map(_.value.toString)
      protobuf.invoice.ItemsDeleted(id.value.toString, protobufItemIds)
    case Protocol.InvoiceCanceled(id) =>
      protobuf.invoice.InvoiceCanceled(id.value.toString)
    case Protocol.InvoiceIssued(id) =>
      protobuf.invoice.InvoiceIssued(id.value.toString)
  }
}

object InvoiceDomainEventPacker {
  val InvoiceCreatedManifest: String = Packer.manifest(protobuf.invoice.InvoiceCreated)
  val ItemsAddedManifest: String = Packer.manifest(protobuf.invoice.ItemsAdded)
  val ItemsDeletedManifest: String = Packer.manifest(protobuf.invoice.ItemsDeleted)
  val InvoiceCanceledManifest: String = Packer.manifest(protobuf.invoice.InvoiceCanceled)
  val InvoiceIssuedManifest: String = Packer.manifest(protobuf.invoice.InvoiceIssued)

  def apply(): InvoiceDomainEventPacker = new InvoiceDomainEventPacker()
}
