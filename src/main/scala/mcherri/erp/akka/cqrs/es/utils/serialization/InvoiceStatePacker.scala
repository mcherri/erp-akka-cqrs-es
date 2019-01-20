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
import mcherri.erp.akka.cqrs.es.utils.serialization.InvoiceStatePacker._
import scalapb.{GeneratedMessage, Message}

class InvoiceStatePacker extends InvoicePacker {
  override def unpack: PartialFunction[any.Any, AnyRef] = {
    case com.google.protobuf.any.Any(`UninitializedInvoiceManifest`, _) =>
      UninitializedInvoice
    case any@com.google.protobuf.any.Any(`EmptyInvoiceManifest`, _) =>
      val protobufEmptyInvoice = any.unpack[protobuf.invoice.EmptyInvoice]
      val protobufClient = protobufEmptyInvoice.client
      val emptyInvoice = for (
        invoiceId <- InvoiceId(UUID.fromString(protobufEmptyInvoice.id));
        personId <- PersonId(protobufClient.id.toLong);
        client <- Client(personId)
      ) yield EmptyInvoice(invoiceId, client)
      emptyInvoice.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`DraftInvoiceManifest`, _) =>
      val protobufDraftInvoice = any.unpack[protobuf.invoice.DraftInvoice]
      val protobufClient = protobufDraftInvoice.client
      val draftInvoice = for (
        invoiceId <- InvoiceId(UUID.fromString(protobufDraftInvoice.id));
        personId <- PersonId(protobufClient.id.toLong);
        client <- Client(personId);
        lineItems <- toLineItems(protobufDraftInvoice.lineItems)
      ) yield DraftInvoice(invoiceId, client, lineItems)
      draftInvoice.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`CanceledInvoiceManifest`, _) =>
      val protobufCanceledInvoice = any.unpack[protobuf.invoice.CanceledInvoice]
      val canceledInvoice = for (
        invoiceId <- InvoiceId(UUID.fromString(protobufCanceledInvoice.id))
      ) yield CanceledInvoice(invoiceId)
      canceledInvoice.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`IssuedInvoiceManifest`, _) =>
      val protobufIssuedInvoice = any.unpack[protobuf.invoice.IssuedInvoice]
      val issuedInvoice = for (
        invoiceId <- InvoiceId(UUID.fromString(protobufIssuedInvoice.id))
      ) yield IssuedInvoice(invoiceId)
      issuedInvoice.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`CanceledIssuedInvoiceManifest`, _) =>
      val protobufCanceledIssuedInvoice = any.unpack[protobuf.invoice.CanceledIssuedInvoice]
      val canceledIssuedInvoice = for (
        invoiceId <- InvoiceId(UUID.fromString(protobufCanceledIssuedInvoice.id))
      ) yield CanceledIssuedInvoice(invoiceId)
      canceledIssuedInvoice.getOrElse(throw new NotSerializableException())
  }

  override def pack: PartialFunction[AnyRef, GeneratedMessage with Message[_]] = {
    case UninitializedInvoice =>
      protobuf.invoice.UninitializedInvoice()
    case EmptyInvoice(id, Client(personId)) =>
      protobuf.invoice.EmptyInvoice(id.value.toString,
        protobuf.person.Client(personId.value.toString))
    case DraftInvoice(id, Client(personId), itemLines) =>
      protobuf.invoice.DraftInvoice(id.value.toString,
        protobuf.person.Client(personId.value.toString),
        toProtobufLineItems(itemLines))
    case CanceledInvoice(id) =>
      protobuf.invoice.CanceledInvoice(id.value.toString)
    case IssuedInvoice(id) =>
      protobuf.invoice.IssuedInvoice(id.value.toString)
    case CanceledIssuedInvoice(id) =>
      protobuf.invoice.CanceledIssuedInvoice(id.value.toString)
  }
}

object InvoiceStatePacker {
  val UninitializedInvoiceManifest: String = Packer.manifest(protobuf.invoice.UninitializedInvoice)
  val EmptyInvoiceManifest: String = Packer.manifest(protobuf.invoice.EmptyInvoice)
  val DraftInvoiceManifest: String = Packer.manifest(protobuf.invoice.DraftInvoice)
  val CanceledInvoiceManifest: String = Packer.manifest(protobuf.invoice.CanceledInvoice)
  val IssuedInvoiceManifest: String = Packer.manifest(protobuf.invoice.IssuedInvoice)
  val CanceledIssuedInvoiceManifest: String = Packer.manifest(protobuf.invoice.CanceledIssuedInvoice)

  def apply(): InvoiceStatePacker = new InvoiceStatePacker()
}
