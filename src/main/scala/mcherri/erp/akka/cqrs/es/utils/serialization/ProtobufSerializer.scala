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
import mcherri.erp.akka.cqrs.es.model.protobuf.invoice
import mcherri.erp.akka.cqrs.es.model.{CanceledInvoice, CanceledIssuedInvoice, Client, DomainEvent, DraftInvoice, EmptyInvoice, Invoice, InvoiceId, IssuedInvoice, ItemId, LineItem, PersonId, RichMoney, State, UninitializedError, UninitializedInvoice, protobuf}
import mcherri.erp.akka.cqrs.es.utils.RichOr._
import mcherri.erp.akka.cqrs.es.utils.serialization.ProtobufSerializer._
import org.scalactic.{Bad, Good, One}
import org.sisioh.baseunits.scala.money.Money
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

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

  private def unpackAny(any: com.google.protobuf.any.Any): AnyRef = {
    any.typeUrl match {
      // Messages
      case GetStateManifest =>
        Aggregate.Protocol.GetState
      case RestartActorManifest =>
        RestartableActor.RestartActor

      // InvoiceCommand
      case CreateInvoiceManifest =>
        val protobufCreateInvoice = any.unpack[protobuf.invoice.CreateInvoice]
        val createInvoice = for (
          personId <- PersonId(protobufCreateInvoice.client.id.toLong);
          client <- Client(personId)
        ) yield InvoiceAggregate.Protocol.CreateInvoice(client)
        createInvoice.getOrElse(throw new NotSerializableException())
      case AddItemsManifest =>
        val protobufAddItems = any.unpack[protobuf.invoice.AddItems]
        val lineItemsSeq = protobufAddItems.lineItems.collect {
          case invoice.LineItem(itemId, code, Some(protobuf.common.Money(amount, currency)), quantity) =>
            ItemId(UUID.fromString(itemId)).flatMap { itemId =>
              LineItem(itemId, code, Money(BigDecimal(amount), Currency.getInstance(currency)), BigDecimal(quantity))
            }
        }.sequence()
        val addItems = for (
          lineItems <- lineItemsSeq
        ) yield InvoiceAggregate.Protocol.AddItems(lineItems)
        addItems.getOrElse(throw new NotSerializableException())
      case DeleteItemsManifest =>
        val protobufDeleteItems = any.unpack[protobuf.invoice.DeleteItems]
        val itemIdsSeq = protobufDeleteItems.itemIds.map { itemId =>
          ItemId(UUID.fromString(itemId))
        }.sequence()
        val deleteItems = for (
          itemIds <- itemIdsSeq
        ) yield InvoiceAggregate.Protocol.DeleteItems(itemIds)
        deleteItems.getOrElse(throw new NotSerializableException())
      case CancelInvoiceManifest =>
        InvoiceAggregate.Protocol.CancelInvoice()
      case IssueInvoiceManifest =>
        InvoiceAggregate.Protocol.IssueInvoice()

      // InvoiceDomainEvent
      case InvoiceCreatedManifest =>
        val protobufInvoiceCreated = any.unpack[protobuf.invoice.InvoiceCreated]
        val protobufClient = protobufInvoiceCreated.client
        val invoiceCreated = for (
          invoiceId <- InvoiceId(UUID.fromString(protobufInvoiceCreated.id));
          personId <- PersonId(protobufClient.id.toLong);
          client <- Client(personId)
        ) yield Protocol.InvoiceCreated(invoiceId, client)
        invoiceCreated.getOrElse(throw new NotSerializableException())
      case ItemsAddedManifest =>
        val protobufItemsAdded = any.unpack[protobuf.invoice.ItemsAdded]
        val lineItemsSeq = protobufItemsAdded.lineItems.collect {
          case invoice.LineItem(itemId, code, Some(protobuf.common.Money(amount, currency)), quantity) =>
            ItemId(UUID.fromString(itemId)).flatMap { itemId =>
              LineItem(itemId, code, Money(BigDecimal(amount), Currency.getInstance(currency)), BigDecimal(quantity))
            }
        }.sequence()
        val itemsAdded = for (
          invoiceId <- InvoiceId(UUID.fromString(protobufItemsAdded.id));
          lineItems <- lineItemsSeq
        ) yield Protocol.ItemsAdded(invoiceId, lineItems)
        itemsAdded.getOrElse(throw new NotSerializableException())
      case ItemsDeletedManifest =>
        val protobufItemsDeleted = any.unpack[protobuf.invoice.ItemsDeleted]
        val itemIdsSeq = protobufItemsDeleted.itemIds.map { itemId =>
          ItemId(UUID.fromString(itemId))
        }.sequence()
        val itemsDeleted = for (
          invoiceId <- InvoiceId(UUID.fromString(protobufItemsDeleted.id));
          itemIds <- itemIdsSeq
        ) yield Protocol.ItemsDeleted(invoiceId, itemIds)
        itemsDeleted.getOrElse(throw new NotSerializableException())
      case InvoiceCanceledManifest =>
        val protobufInvoiceCanceled = any.unpack[protobuf.invoice.InvoiceCanceled]
        val invoiceCanceled = for (
          invoiceId <- InvoiceId(UUID.fromString(protobufInvoiceCanceled.id))
        ) yield Protocol.InvoiceCanceled(invoiceId)
        invoiceCanceled.getOrElse(throw new NotSerializableException())
      case InvoiceIssuedManifest =>
        val protobufInvoiceIssued = any.unpack[protobuf.invoice.InvoiceIssued]
        val invoiceIssued = for (
          invoiceId <- InvoiceId(UUID.fromString(protobufInvoiceIssued.id))
        ) yield Protocol.InvoiceCanceled(invoiceId)
        invoiceIssued.getOrElse(throw new NotSerializableException())

      // InvoiceState
      case UninitializedInvoiceManifest =>
        UninitializedInvoice
      case EmptyInvoiceManifest =>
        val protobufEmptyInvoice = any.unpack[protobuf.invoice.EmptyInvoice]
        val protobufClient = protobufEmptyInvoice.client
        val emptyInvoice = for (
          invoiceId <- InvoiceId(UUID.fromString(protobufEmptyInvoice.id));
          personId <- PersonId(protobufClient.id.toLong);
          client <- Client(personId)
        ) yield EmptyInvoice(invoiceId, client)
        emptyInvoice.getOrElse(throw new NotSerializableException())
      case DraftInvoiceManifest =>
        val protobufDraftInvoice = any.unpack[protobuf.invoice.DraftInvoice]
        val protobufClient = protobufDraftInvoice.client
        val lineItemsSeq = protobufDraftInvoice.lineItems.collect {
          case invoice.LineItem(itemId, code, Some(protobuf.common.Money(amount, currency)), quantity) =>
            ItemId(UUID.fromString(itemId)).flatMap { itemId =>
              LineItem(itemId, code, Money(BigDecimal(amount), Currency.getInstance(currency)), BigDecimal(quantity))
            }
        }.sequence()
        val draftInvoice = for (
          invoiceId <- InvoiceId(UUID.fromString(protobufDraftInvoice.id));
          personId <- PersonId(protobufClient.id.toLong);
          client <- Client(personId);
          lineItems <- lineItemsSeq
        ) yield DraftInvoice(invoiceId, client, lineItems)
        draftInvoice.getOrElse(throw new NotSerializableException())
      case CanceledInvoiceManifest =>
        val protobufCanceledInvoice = any.unpack[protobuf.invoice.CanceledInvoice]
        val canceledInvoice = for (
          invoiceId <- InvoiceId(UUID.fromString(protobufCanceledInvoice.id))
        ) yield CanceledInvoice(invoiceId)
        canceledInvoice.getOrElse(throw new NotSerializableException())
      case IssuedInvoiceManifest =>
        val protobufIssuedInvoice = any.unpack[protobuf.invoice.IssuedInvoice]
        val issuedInvoice = for (
          invoiceId <- InvoiceId(UUID.fromString(protobufIssuedInvoice.id))
        ) yield CanceledInvoice(invoiceId)
        issuedInvoice.getOrElse(throw new NotSerializableException())
      case CanceledIssuedInvoiceManifest =>
        val protobufCanceledIssuedInvoice = any.unpack[protobuf.invoice.CanceledIssuedInvoice]
        val canceledIssuedInvoice = for (
          invoiceId <- InvoiceId(UUID.fromString(protobufCanceledIssuedInvoice.id))
        ) yield CanceledIssuedInvoice(invoiceId)
        canceledIssuedInvoice.getOrElse(throw new NotSerializableException())

      // InvoiceError
      case ItemIdsNotFoundErrorManifest =>
        val protobufError = any.unpack[protobuf.invoice.ItemIdsNotFoundError]
        val itemIdsSeq = protobufError.itemIds.map { itemId =>
          ItemId(UUID.fromString(itemId))
        }.sequence()
        val itemIdsNotFoundError = for (
          invoiceId <- InvoiceId(UUID.fromString(protobufError.id));
          itemIds <- itemIdsSeq
        ) yield Invoice.ItemIdsNotFoundError(invoiceId, itemIds)
        itemIdsNotFoundError.getOrElse(throw new NotSerializableException())
      case AlreadyCanceledErrorManifest =>
        val protobufError = any.unpack[protobuf.invoice.AlreadyCanceledError]
        val alreadyCanceledError = for (
          invoiceId <- InvoiceId(UUID.fromString(protobufError.id))
        ) yield Invoice.AlreadyCanceledError(invoiceId)
        alreadyCanceledError.getOrElse(throw new NotSerializableException())
      case AlreadyIssuedErrorManifest =>
        val protobufError = any.unpack[protobuf.invoice.AlreadyIssuedError]
        val alreadyIssuedError = for (
          invoiceId <- InvoiceId(UUID.fromString(protobufError.id))
        ) yield Invoice.AlreadyIssuedError(invoiceId)
        alreadyIssuedError.getOrElse(throw new NotSerializableException())
      case EmptyInvoiceErrorManifest =>
        val protobufError = any.unpack[protobuf.invoice.EmptyInvoiceError]
        val emptyInvoiceError = for (
          invoiceId <- InvoiceId(UUID.fromString(protobufError.id))
        ) yield Invoice.EmptyInvoiceError(invoiceId)
        emptyInvoiceError.getOrElse(throw new NotSerializableException())
      case UninitializedInvoiceErrorManifest =>
        Invoice.UninitializedInvoiceError
      case AlreadyInitializedErrorManifest =>
        Invoice.AlreadyInitializedError

      // Errors
      case UninitializedErrorManifest =>
        val protobufError = any.unpack[protobuf.error.UninitializedError]
        UninitializedError(protobufError.id)

      // scalatic
      case GoodManifest =>
        val protobufGood = any.unpack[protobuf.scalactic.Good]
        Good(unpackAny(protobufGood.g))
      case BadManifest =>
        val protobufBad = any.unpack[protobuf.scalactic.Bad]
        Bad(unpackAny(protobufBad.b))
      case OneManifest =>
        val protobufOne = any.unpack[protobuf.scalactic.One]
        One(unpackAny(protobufOne.loneElement))

      // Dummies for testing
      case DummyEventManifest => DummyEvent
      case DummyValidCommandManifest => DummyValidCommand
      case DummyInvalidCommandManifest => DummyInvalidCommand
      case DummyErrorManifest => DummyError
      case DummyStateManifest => DummyState
      case _ => throw new NotSerializableException(any.typeUrl)
    }
  }

  override def toBinary(o: AnyRef): Array[Byte] = pack(toProtobuf(o)).toByteArray

  private def toProtobuf(o : AnyRef): GeneratedMessage with Message[_] = o match {
    // Messages
    case Aggregate.Protocol.GetState =>
      protobuf.command.GetState()
    case RestartableActor.RestartActor =>
      protobuf.command.RestartActor()

    // InvoiceCommand
    case InvoiceAggregate.Protocol.CreateInvoice(Client(personId)) =>
      protobuf.invoice.CreateInvoice(protobuf.person.Client(personId.value.toString))
    case InvoiceAggregate.Protocol.AddItems(lineItems) =>
      val protoBufLineItems = lineItems.map {
        case LineItem(ItemId(itemId), code, money /*Money(amount, currency)*/, quantity) =>
          protobuf.invoice.LineItem(itemId.toString, code,
            Some(money.toProtobuf), quantity.toString())
      }
      protobuf.invoice.AddItems(protoBufLineItems)
    case InvoiceAggregate.Protocol.DeleteItems(itemIds) =>
      val protobufItemIds = itemIds.map(_.value.toString)
      protobuf.invoice.DeleteItems(protobufItemIds)
    case InvoiceAggregate.Protocol.CancelInvoice() =>
      protobuf.invoice.CancelInvoice()
    case InvoiceAggregate.Protocol.IssueInvoice() =>
      protobuf.invoice.IssueInvoice()

    // InvoiceDomainEvent
    case Protocol.InvoiceCreated(id, Client(personId)) =>
      protobuf.invoice.InvoiceCreated(id.value.toString,
        protobuf.person.Client(personId.value.toString))
    case Protocol.ItemsAdded(id, lineItems) =>
      val protoBufLineItems = lineItems.map {
        case LineItem(ItemId(itemId), code, money /*Money(amount, currency)*/, quantity) =>
          protobuf.invoice.LineItem(itemId.toString, code,
            Some(money.toProtobuf), quantity.toString())
      }
      protobuf.invoice.ItemsAdded(id.value.toString, protoBufLineItems)
    case Protocol.ItemsDeleted(id, itemIds) =>
      val protobufItemIds = itemIds.map(_.value.toString)
      protobuf.invoice.ItemsDeleted(id.value.toString, protobufItemIds)
    case Protocol.InvoiceCanceled(id) =>
      protobuf.invoice.InvoiceCanceled(id.value.toString)
    case Protocol.InvoiceIssued(id) =>
      protobuf.invoice.InvoiceIssued(id.value.toString)

    // InvoiceState
    case UninitializedInvoice =>
      protobuf.invoice.UninitializedInvoice()
    case EmptyInvoice(id, Client(personId)) =>
      protobuf.invoice.EmptyInvoice(id.value.toString,
        protobuf.person.Client(personId.value.toString))
    case DraftInvoice(id, Client(personId), itemLines) =>
      val protoBufItemLines = itemLines.map {
        case LineItem(ItemId(itemId), code, money /*Money(amount, currency)*/, quantity) =>
          protobuf.invoice.LineItem(itemId.toString, code,
            Some(money.toProtobuf), quantity.toString())
      }
      protobuf.invoice.DraftInvoice(id.value.toString,
        protobuf.person.Client(personId.value.toString), protoBufItemLines)
    case CanceledInvoice(id) =>
      protobuf.invoice.CanceledInvoice(id.value.toString)
    case IssuedInvoice(id) =>
      protobuf.invoice.IssuedInvoice(id.value.toString)
    case CanceledIssuedInvoice(id) =>
      protobuf.invoice.CanceledIssuedInvoice(id.value.toString)

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

    // Errors
    case UninitializedError(id) =>
      protobuf.error.UninitializedError(id)

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

  def pack[A <: GeneratedMessage](generatedMessage: A): com.google.protobuf.any.Any =
    com.google.protobuf.any.Any(
      typeUrl = manifest(generatedMessage.companion),
      value = generatedMessage.toByteString
    )

}

object ProtobufSerializer {

  val Prefix = "type.googleapis.com/"

  private def manifest(companion: GeneratedMessageCompanion[_]): String = Prefix + companion.scalaDescriptor.fullName

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
  case object DummyValidCommand extends Command

  case object DummyInvalidCommand extends Command

  case object DummyEvent extends DomainEvent

  case object DummyError extends model.Error("An error")

  case object DummyState extends State

  val DummyValidCommandManifest: String = manifest(protobuf.dummy.DummyValidCommand)
  val DummyInvalidCommandManifest: String = manifest(protobuf.dummy.DummyInvalidCommand)
  val DummyEventManifest: String = manifest(protobuf.dummy.DummyEvent)
  val DummyErrorManifest: String = manifest(protobuf.dummy.DummyError)
  val DummyStateManifest: String = manifest(protobuf.dummy.DummyState)
}