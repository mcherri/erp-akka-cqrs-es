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
import java.util.{Currency, UUID}

import akka.serialization.Serializer
import mcherri.erp.akka.cqrs.es.aggregate.{Aggregate, Command, InvoiceAggregate, RestartableActor}
import mcherri.erp.akka.cqrs.es.model
import mcherri.erp.akka.cqrs.es.model.Invoice.Protocol
import mcherri.erp.akka.cqrs.es.model._
import mcherri.erp.akka.cqrs.es.model.protobuf.invoice
import mcherri.erp.akka.cqrs.es.utils.RichOr._
import mcherri.erp.akka.cqrs.es.utils.serialization.ProtobufSerializer._
import org.scalactic._
import org.sisioh.baseunits.scala.money.Money
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

import scala.collection.immutable

/*
 * TODO: It is obvious the code below needs a framework somehow.
 * I may or may not use https://github.com/bfil/scala-automapper. It is important to
 * refactor this class as it will play an important role in schema evolution later.
 */
class ProtobufSerializer extends Serializer {
  override def identifier: Int = 399697686

  override def includeManifest: Boolean = false

  override def fromBinary(bytes: Array[Byte], manifest: Option[Class[_]]): AnyRef =
    unpackAny(com.google.protobuf.any.Any.parseFrom(bytes))

  override def toBinary(o: AnyRef): Array[Byte] = pack(toProtobuf(o)).toByteArray

  def pack[A <: GeneratedMessage](generatedMessage: A): com.google.protobuf.any.Any =
    com.google.protobuf.any.Any(
      typeUrl = manifest(generatedMessage.companion),
      value = generatedMessage.toByteString
    )

  private def toItemIds(protobufIds: immutable.Seq[String]): Or[immutable.Seq[ItemId], Every[ItemId.ItemIdError]] = {
    protobufIds.map { itemId =>
      ItemId(UUID.fromString(itemId))
    }.sequence()
  }

  private def toLineItems(lineItems: immutable.Seq[invoice.LineItem]): Or[immutable.Seq[LineItem], Every[Error]] = {
    lineItems.collect {
      case invoice.LineItem(itemId, code, Some(protobuf.common.Money(amount, currency)), quantity) =>
        ItemId(UUID.fromString(itemId)).flatMap { itemId =>
          LineItem(itemId, code, Money(BigDecimal(amount), Currency.getInstance(currency)), BigDecimal(quantity))
        }
    }.sequence()
  }

  private def unpackMessage: PartialFunction[com.google.protobuf.any.Any, AnyRef] = {
    // Messages
    case com.google.protobuf.any.Any(`GetStateManifest`, _) =>
      Aggregate.Protocol.GetState
    case com.google.protobuf.any.Any(`RestartActorManifest`, _) =>
      RestartableActor.RestartActor
  }

  private def unpackInvoiceCommand: PartialFunction[com.google.protobuf.any.Any, AnyRef] = {
    // InvoiceCommand
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

  private def unpackInvoiceDomainEvent: PartialFunction[com.google.protobuf.any.Any, AnyRef] = {
    // InvoiceDomainEvent
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

  private def unpackInvoiceState: PartialFunction[com.google.protobuf.any.Any, AnyRef] = {
    // InvoiceState
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
      ) yield CanceledInvoice(invoiceId)
      issuedInvoice.getOrElse(throw new NotSerializableException())
    case any@com.google.protobuf.any.Any(`CanceledIssuedInvoiceManifest`, _) =>
      val protobufCanceledIssuedInvoice = any.unpack[protobuf.invoice.CanceledIssuedInvoice]
      val canceledIssuedInvoice = for (
        invoiceId <- InvoiceId(UUID.fromString(protobufCanceledIssuedInvoice.id))
      ) yield CanceledIssuedInvoice(invoiceId)
      canceledIssuedInvoice.getOrElse(throw new NotSerializableException())
  }

  private def unpackInvoiceError: PartialFunction[com.google.protobuf.any.Any, AnyRef] = {
    // InvoiceError
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

  private def unpackErrors: PartialFunction[com.google.protobuf.any.Any, AnyRef] = {
    // Errors
    case any@com.google.protobuf.any.Any(`UninitializedErrorManifest`, _) =>
      val protobufError = any.unpack[protobuf.error.UninitializedError]
      UninitializedError(protobufError.id)
  }

  private def unpackScalatic: PartialFunction[com.google.protobuf.any.Any, AnyRef] = {
    // scalatic
    case any@com.google.protobuf.any.Any(`GoodManifest`, _) =>
      val protobufGood = any.unpack[protobuf.scalactic.Good]
      Good(unpackAny(protobufGood.g))
    case any@com.google.protobuf.any.Any(`BadManifest`, _) =>
      val protobufBad = any.unpack[protobuf.scalactic.Bad]
      Bad(unpackAny(protobufBad.b))
    case any@com.google.protobuf.any.Any(`OneManifest`, _) =>
      val protobufOne = any.unpack[protobuf.scalactic.One]
      One(unpackAny(protobufOne.loneElement))
  }

  private def unpackDummies: PartialFunction[com.google.protobuf.any.Any, AnyRef] = {
    // Dummies for testing
    case com.google.protobuf.any.Any(`DummyEventManifest`, _) => DummyEvent
    case com.google.protobuf.any.Any(`DummyValidCommandManifest`, _) => DummyValidCommand
    case com.google.protobuf.any.Any(`DummyInvalidCommandManifest`, _) => DummyInvalidCommand
    case com.google.protobuf.any.Any(`DummyErrorManifest`, _) => DummyError
    case com.google.protobuf.any.Any(`DummyStateManifest`, _) => DummyState
  }

  private def throwNotSerializableException: PartialFunction[com.google.protobuf.any.Any, AnyRef] = {
    case any: com.google.protobuf.any.Any => throw new NotSerializableException(any.typeUrl)
    case _ => throw new NotSerializableException()
  }

  private def unpackAny: PartialFunction[com.google.protobuf.any.Any, AnyRef] = {
    unpackMessage orElse
      unpackInvoiceCommand orElse
      unpackInvoiceDomainEvent orElse
      unpackInvoiceState orElse
      unpackInvoiceError orElse
      unpackErrors orElse
      unpackScalatic orElse
      unpackDummies orElse
      throwNotSerializableException
  }

  private def toProtobufLineItems(lineItems: immutable.Seq[LineItem]): immutable.Seq[invoice.LineItem] = {
    lineItems.map {
      case LineItem(ItemId(itemId), code, money, quantity) =>
        protobuf.invoice.LineItem(itemId.toString, code,
          Some(money.toProtobuf), quantity.toString())
    }
  }

  private def toProtobufMessages: PartialFunction[AnyRef, GeneratedMessage with Message[_]] = {
    // Messages
    case Aggregate.Protocol.GetState =>
      protobuf.command.GetState()
    case RestartableActor.RestartActor =>
      protobuf.command.RestartActor()
  }

  private def toProtobufInvoiceCommand: PartialFunction[AnyRef, GeneratedMessage with Message[_]] = {
    // InvoiceCommand
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

  private def toProtobufInvoiceDomainEvent: PartialFunction[AnyRef, GeneratedMessage with Message[_]] = {
    // InvoiceDomainEvent
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

  private def toProtobufInvoiceState: PartialFunction[AnyRef, GeneratedMessage with Message[_]] = {
    // InvoiceState
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

  private def toProtobufInvoiceError: PartialFunction[AnyRef, GeneratedMessage with Message[_]] = {
    // InvoiceError
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

  private def toProtobufErrors: PartialFunction[AnyRef, GeneratedMessage with Message[_]] = {
    // Errors
    case UninitializedError(id) =>
      protobuf.error.UninitializedError(id)
  }

  private def toProtobufScalatic: PartialFunction[AnyRef, GeneratedMessage with Message[_]] = {
    // scalatic
    case Good(anyRef: AnyRef) =>
      val message = toProtobuf(anyRef)
      protobuf.scalactic.Good(pack(message))
    case Bad(anyRef: AnyRef) =>
      val message = toProtobuf(anyRef)
      protobuf.scalactic.Bad(pack(message))
    case One(anyRef: AnyRef) =>
      val message = toProtobuf(anyRef)
      protobuf.scalactic.One(pack(message))
  }

  private def toProtobufDummies: PartialFunction[AnyRef, GeneratedMessage with Message[_]] = {
    // Dummies for testing
    case DummyEvent =>
      protobuf.dummy.DummyEvent()
    case DummyValidCommand =>
      protobuf.dummy.DummyValidCommand()
    case DummyInvalidCommand =>
      protobuf.dummy.DummyInvalidCommand()
    case DummyError =>
      protobuf.dummy.DummyError()
    case DummyState =>
      protobuf.dummy.DummyState()
  }

  private def toProtobuf: PartialFunction[AnyRef, GeneratedMessage with Message[_]] = {
    toProtobufMessages orElse
      toProtobufInvoiceCommand orElse
      toProtobufInvoiceDomainEvent orElse
      toProtobufInvoiceState orElse
      toProtobufInvoiceError orElse
      toProtobufErrors orElse
      toProtobufScalatic orElse
      toProtobufDummies
  }

}

object ProtobufSerializer {

  val Prefix = "type.googleapis.com/"
  // Messages
  val GetStateManifest: String = manifest(protobuf.command.GetState)
  val RestartActorManifest: String = manifest(protobuf.command.RestartActor)
  // InvoiceCommand
  val CreateInvoiceManifest: String = manifest(protobuf.invoice.CreateInvoice)
  val AddItemsManifest: String = manifest(protobuf.invoice.AddItems)
  val DeleteItemsManifest: String = manifest(protobuf.invoice.DeleteItems)
  val CancelInvoiceManifest: String = manifest(protobuf.invoice.CancelInvoice)
  val IssueInvoiceManifest: String = manifest(protobuf.invoice.IssueInvoice)
  // InvoiceDomainEvent
  val InvoiceCreatedManifest: String = manifest(protobuf.invoice.InvoiceCreated)
  val ItemsAddedManifest: String = manifest(protobuf.invoice.ItemsAdded)
  val ItemsDeletedManifest: String = manifest(protobuf.invoice.ItemsDeleted)
  val InvoiceCanceledManifest: String = manifest(protobuf.invoice.InvoiceCanceled)
  val InvoiceIssuedManifest: String = manifest(protobuf.invoice.InvoiceIssued)
  // InvoiceState
  val UninitializedInvoiceManifest: String = manifest(protobuf.invoice.UninitializedInvoice)
  val EmptyInvoiceManifest: String = manifest(protobuf.invoice.EmptyInvoice)
  val DraftInvoiceManifest: String = manifest(protobuf.invoice.DraftInvoice)
  val CanceledInvoiceManifest: String = manifest(protobuf.invoice.CanceledInvoice)
  val IssuedInvoiceManifest: String = manifest(protobuf.invoice.IssuedInvoice)
  val CanceledIssuedInvoiceManifest: String = manifest(protobuf.invoice.CanceledIssuedInvoice)
  // InvoiceError
  val ItemIdsNotFoundErrorManifest: String = manifest(protobuf.invoice.ItemIdsNotFoundError)
  val AlreadyCanceledErrorManifest: String = manifest(protobuf.invoice.AlreadyCanceledError)
  val AlreadyIssuedErrorManifest: String = manifest(protobuf.invoice.AlreadyIssuedError)
  val EmptyInvoiceErrorManifest: String = manifest(protobuf.invoice.EmptyInvoiceError)
  val UninitializedInvoiceErrorManifest: String = manifest(protobuf.invoice.UninitializedInvoiceError)
  val AlreadyInitializedErrorManifest: String = manifest(protobuf.invoice.AlreadyInitializedError)
  // Errors
  val UninitializedErrorManifest: String = manifest(protobuf.error.UninitializedError)
  // scalatic
  val GoodManifest: String = manifest(protobuf.scalactic.Good)
  val BadManifest: String = manifest(protobuf.scalactic.Bad)
  val OneManifest: String = manifest(protobuf.scalactic.One)
  // Dummies for testing
  val DummyValidCommandManifest: String = manifest(protobuf.dummy.DummyValidCommand)
  val DummyInvalidCommandManifest: String = manifest(protobuf.dummy.DummyInvalidCommand)
  val DummyEventManifest: String = manifest(protobuf.dummy.DummyEvent)
  val DummyErrorManifest: String = manifest(protobuf.dummy.DummyError)
  val DummyStateManifest: String = manifest(protobuf.dummy.DummyState)

  private def manifest(companion: GeneratedMessageCompanion[_]): String = Prefix + companion.scalaDescriptor.fullName

  // Dummies for testing
  case object DummyValidCommand extends Command

  case object DummyInvalidCommand extends Command

  case object DummyEvent extends DomainEvent

  case object DummyError extends model.Error("An error")

  case object DummyState extends State

}