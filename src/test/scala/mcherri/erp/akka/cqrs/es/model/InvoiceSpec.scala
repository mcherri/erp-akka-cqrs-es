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
package mcherri.erp.akka.cqrs.es.model

import java.util.UUID

import mcherri.erp.akka.cqrs.es.UnitSpec
import org.scalactic.{Every, Or}
import org.sisioh.baseunits.scala.money

import scala.collection.immutable.Seq

class InvoiceSpec extends UnitSpec {

  trait LineItemFixture extends CommonFixture {
    protected val negativeEur = money.Money(BigDecimal("-2.00"), money.Money.EUR)
    protected val zeroEur = money.Money(BigDecimal("0.00"), money.Money.EUR)
    protected val positiveEur = money.Money(BigDecimal("2.00"), money.Money.EUR)
    protected val id = ItemId(UUID.randomUUID())
    protected val item: Item Or Every[Error] = id.flatMap(id => Item(id, "", "", oneEur, BigDecimal("1")))
  }

  "A line item" should "reject negative price value" in new LineItemFixture {
    item.flatMap(item => LineItem(item.id, item.code, negativeEur, 1)) shouldBe 'bad
  }

  it should "accept zero price value" in new LineItemFixture {
    item.flatMap(item => LineItem(item.id, item.code, zeroEur, 1)) shouldBe 'good
  }

  it should "accept positive price value" in new LineItemFixture {
    item.flatMap(item => LineItem(item.id, item.code, positiveEur, 1)) shouldBe 'good
  }

  it should "accept positive quantity" in new LineItemFixture {
    item.flatMap(item => LineItem(item.id, item.code, oneEur, 1)) shouldBe 'good
  }

  it should "reject zero quantity" in new LineItemFixture {
    item.flatMap(item => LineItem(item.id, item.code, oneEur, 0)) shouldBe 'bad
  }

  it should "reject negative quantity" in new LineItemFixture {
    item.flatMap(item => LineItem(item.id, item.code, oneEur, -1)) shouldBe 'bad
  }

  // TODO: Re-enable this test once we have the inventory Saga
  it should "accept quantity <= remainingQuantity" ignore new LineItemFixture {
    item.flatMap(item => LineItem(item.id, item.code, oneEur, 1)) shouldBe 'good
    id.flatMap(id => LineItem(id, "", oneEur, 1)) shouldBe 'good
  }

  // TODO: Re-enable this test once we have the inventory Saga
  it should "reject quantity > remainingQuantity" ignore new LineItemFixture {
    item.flatMap(item => LineItem(item.id, item.code, oneEur, 2)) shouldBe 'bad
  }

  "An invoice" should "allow adding new items" in new InvoiceFixture {
    invoice shouldBe 'good
  }

  it should "merge line items having the same item" in new InvoiceFixture {
    private val result = for (
      state1 <- invoice;
      lineItemSeq1 <- anotherLineItemSeq;
      state2 <- state1.add(lineItemSeq1)
    ) yield state2

    result shouldBe 'good
    result.foreach {
      case state: DraftInvoice =>
      state.itemLines should have size 10
    }
  }

  it should "allow deleting existing items" in new InvoiceFixture {
    private val result = for (
      state1 <- invoice;
      itemIds1 <- itemIdsToDelete;
      state2 <- state1.delete(itemIds1)
    ) yield state2

    result shouldBe 'good
    result.foreach {
      case state: DraftInvoice =>
        state.itemLines should have size 8
    }
  }

  it should "disallow deleting non-existing items" in new InvoiceFixture {
    private val itemId = ItemId(UUID.randomUUID())

    private val result = for (
      state1 <- invoice;
      itemId1 <- itemId;
      state2 <- state1.delete(Seq(itemId1))
    ) yield state2

    result shouldBe 'bad
  }

  it can "be canceled" in new InvoiceFixture {
    canceledInvoice shouldBe 'good
  }

  it can "be canceled even if empty" in new InvoiceFixture {
    private val result = for (
      state1 <- emptyInvoice;
      state2 <- state1.cancel()
    ) yield state2

    result shouldBe 'good
  }

  it should "be canceled once" in new InvoiceFixture {
    private val result = for (
      state1 <- canceledInvoice;
      state2 <- state1.cancel()
    ) yield state2

    result shouldBe 'bad
  }

  it should "not allow adding items when canceled" in new InvoiceFixture {
    private val result = for (
      state1 <- canceledInvoice;
      lineItemSeq1 <- anotherLineItemSeq;
      state2 <- state1.add(lineItemSeq1)
    ) yield state2

    result shouldBe 'bad
  }

  it should "not allow deleting items when canceled" in new InvoiceFixture {
    private val result = for (
      state1 <- canceledInvoice;
      itemIds1 <- itemIdsToDelete;
      state2 <- state1.delete(itemIds1)
    ) yield state2

    result shouldBe 'bad
  }

  it can "be issued" in new InvoiceFixture {
    issuedInvoice shouldBe 'good
  }

  it should "be issued once" in new InvoiceFixture {
    private val result = for (
      state1 <- issuedInvoice;
      state2 <- state1.issue()
    ) yield state2

    result shouldBe 'bad
  }

  it should "have a least one line item when being issued" in new InvoiceFixture {
    private val result = for (
      state1 <- emptyInvoice;
      state2 <- state1.issue()
    ) yield state2

    result shouldBe 'bad
  }

  it should "not allow adding items after being issued" in new InvoiceFixture {
    private val result = for (
      state1 <- issuedInvoice;
      lineItemSeq1 <- anotherLineItemSeq;
      state2 <- state1.add(lineItemSeq1)
    ) yield state2

    result shouldBe 'bad
  }

  it should "not allow deleting items after being issued" in new InvoiceFixture {
    private val result = for (
      state1 <- issuedInvoice;
      itemIds1 <- itemIdsToDelete;
      state2 <- state1.delete(itemIds1)
    ) yield state2

    result shouldBe 'bad
  }

  it can "be cancel after being issued" in new InvoiceFixture {
    private val result = for (
      state1 <- issuedInvoice;
      state2 <- state1.cancel()
    ) yield state2

    result shouldBe 'good
  }

}
