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
import mcherri.erp.akka.cqrs.es.model.{Invoice, InvoiceId, protobuf}
import mcherri.erp.akka.cqrs.es.utils.serialization.InvoiceErrorPacker._
import scalapb.{GeneratedMessage, Message}

class InvoiceErrorPacker extends InvoicePacker {
  override def unpack: PartialFunction[any.Any, AnyRef] = {
    case any@com.google.protobuf.any.Any(`ItemIdsNotFoundErrorManifest`, _) =>
      val protobufError = any.unpack[protobuf.invoice.ItemIdsNotFoundError]
      val itemIdsNotFoundError = for (
        invoiceId <- InvoiceId(UUID.fromString(protobufError.id));
        itemIds <- toItemIds(protobufError.itemIds)
      ) yield Invoice.ItemIdsNotFoundError(invoiceId, itemIds)
      itemIdsNotFoundError.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`AlreadyCanceledErrorManifest`, _) =>
      val protobufError = any.unpack[protobuf.invoice.AlreadyCanceledError]
      val alreadyCanceledError = for (
        invoiceId <- InvoiceId(UUID.fromString(protobufError.id))
      ) yield Invoice.AlreadyCanceledError(invoiceId)
      alreadyCanceledError.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`AlreadyIssuedErrorManifest`, _) =>
      val protobufError = any.unpack[protobuf.invoice.AlreadyIssuedError]
      val alreadyIssuedError = for (
        invoiceId <- InvoiceId(UUID.fromString(protobufError.id))
      ) yield Invoice.AlreadyIssuedError(invoiceId)
      alreadyIssuedError.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`EmptyInvoiceErrorManifest`, _) =>
      val protobufError = any.unpack[protobuf.invoice.EmptyInvoiceError]
      val emptyInvoiceError = for (
        invoiceId <- InvoiceId(UUID.fromString(protobufError.id))
      ) yield Invoice.EmptyInvoiceError(invoiceId)
      emptyInvoiceError.getOrElse(throw new NotSerializableException())
    case com.google.protobuf.any.Any(`UninitializedInvoiceErrorManifest`, _) =>
      Invoice.UninitializedInvoiceError
    case com.google.protobuf.any.Any(`AlreadyInitializedErrorManifest`, _) =>
      Invoice.AlreadyInitializedError
  }

  override def pack: PartialFunction[AnyRef, GeneratedMessage with Message[_]] = {
    case Invoice.ItemIdsNotFoundError(id, itemIds) =>
      val protobufItemIds = itemIds.map(_.value.toString)
      protobuf.invoice.ItemIdsNotFoundError(id.value.toString, protobufItemIds)
    case Invoice.AlreadyCanceledError(id) =>
      protobuf.invoice.AlreadyCanceledError(id.value.toString)
    case Invoice.AlreadyIssuedError(id) =>
      protobuf.invoice.AlreadyIssuedError(id.value.toString)
    case Invoice.EmptyInvoiceError(id) =>
      protobuf.invoice.EmptyInvoiceError(id.value.toString)
    case Invoice.UninitializedInvoiceError =>
      protobuf.invoice.UninitializedInvoiceError()
    case Invoice.AlreadyInitializedError =>
      protobuf.invoice.AlreadyInitializedError()
  }
}

object InvoiceErrorPacker {
  val ItemIdsNotFoundErrorManifest: String = Packer.manifest(protobuf.invoice.ItemIdsNotFoundError)
  val AlreadyCanceledErrorManifest: String = Packer.manifest(protobuf.invoice.AlreadyCanceledError)
  val AlreadyIssuedErrorManifest: String = Packer.manifest(protobuf.invoice.AlreadyIssuedError)
  val EmptyInvoiceErrorManifest: String = Packer.manifest(protobuf.invoice.EmptyInvoiceError)
  val UninitializedInvoiceErrorManifest: String = Packer.manifest(protobuf.invoice.UninitializedInvoiceError)
  val AlreadyInitializedErrorManifest: String = Packer.manifest(protobuf.invoice.AlreadyInitializedError)

  def apply(): InvoiceErrorPacker = new InvoiceErrorPacker()
}