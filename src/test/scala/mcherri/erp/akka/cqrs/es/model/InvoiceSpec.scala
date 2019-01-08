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
import mcherri.erp.akka.cqrs.es.utils.RichOr._
import org.scalactic.{Every, Or}
import org.sisioh.baseunits.scala.money.Money

class InvoiceSpec extends UnitSpec {

  trait CommonFixture {
    protected val oneEur = Money(BigDecimal("1.00"), Money.EUR)
  }

  trait LineItemFixture extends CommonFixture {
    protected val negativeEur = Money(BigDecimal("-2.00"), Money.EUR)
    protected val zeroEur = Money(BigDecimal("0.00"), Money.EUR)
    protected val positiveEur = Money(BigDecimal("2.00"), Money.EUR)
    protected val id = ItemId(UUID.randomUUID())
    protected val item: Item Or Every[ItemIdError] = id.map(Item(_, "", "", oneEur, 1))
  }

  "A line item" should "reject negative price value" in new LineItemFixture {
    assert(item.flatMap(LineItem(_, negativeEur, 1)).isBad)
  }

  it should "accept zero price value" in new LineItemFixture {
    assert(item.flatMap(LineItem(_, zeroEur, 1)).isGood)
  }

  it should "accept positive price value" in new LineItemFixture {
    assert(item.flatMap(LineItem(_, positiveEur, 1)).isGood)
  }

  it should "accept positive quantity" in new LineItemFixture {
    assert(item.flatMap(LineItem(_, oneEur, 1)).isGood)
  }

  it should "reject zero quantity" in new LineItemFixture {
    assert(item.flatMap(LineItem(_, oneEur, 0)).isBad)
  }

  it should "reject negative quantity" in new LineItemFixture {
    assert(item.flatMap(LineItem(_, oneEur, -1)).isBad)
  }

  it should "accept quantity <= remainingQuantity" in new LineItemFixture {
    assert(item.flatMap(LineItem(_, oneEur, 1)).isGood)
    assert(id.flatMap(id => LineItem(Item(id, "", "", oneEur, 2), oneEur, 1)).isGood)
  }

  it should "reject quantity > remainingQuantity" in new LineItemFixture {
    assert(item.flatMap(LineItem(_, oneEur, 2)).isBad)
  }

  trait InvoiceFixture extends CommonFixture {
    private val id = InvoiceId(UUID.randomUUID())
    private val client = PersonId(1).map(Client)
    protected val emptyInvoice: Or[Invoice, Every[Error]] = for (
      id1 <- id;
      client1 <- client;
      invoice1 <- Invoice(id1, client1)
    ) yield invoice1
    protected val lineItemSeq: Or[Seq[LineItem], Every[Error]] = (1 to 10).map { i =>
      val price = oneEur.times(Math.random() * 100)
      val quantity = Math.random() * 100 + 1
      ItemId(UUID.randomUUID()).flatMap(id => LineItem(Item(id, s"abc$i", "description $i", price, quantity + 11), price, quantity))
    }.sequence()
    protected val invoice: Invoice Or Every[Error] = for (
      invoice1 <- emptyInvoice;
      lineItemSeq1 <- lineItemSeq;
      invoice2 <- invoice1.add(lineItemSeq1)
    ) yield invoice2
  }

  "An invoice" should "allow adding new items" in new InvoiceFixture {
    assert(invoice.isGood)
  }

  it should "merge line items having the same item" in new InvoiceFixture {
    private val item = lineItemSeq.flatMap(_ (4).copy(quantity = 10))
    private val anotherLineItemSeq = item.map(Seq(_))
    private val result: Invoice Or Every[Error] = for (
      invoice1 <- invoice;
      lineItemSeq1 <- anotherLineItemSeq;
      invoice2 <- invoice1.add(lineItemSeq1)
    ) yield invoice2

    assert(result.isGood)
    result.foreach { invoice1 =>
      assert(invoice1.count == 10)
    }
  }

  it should "allow deleting existing items" in new InvoiceFixture {
    private val itemIds = lineItemSeq.map(l => Seq(l(5).item.id, l(8).item.id))
    private val result = for (
      invoice1 <- invoice;
      itemIds1 <- itemIds;
      invoice2 <- invoice1.delete(itemIds1)
    ) yield invoice2

    assert(result.isGood)
    result.foreach { invoice1 =>
      assert(invoice1.count == 8)
    }
  }

  it should "disallow deleting non-existing items" in new InvoiceFixture {
    private val itemId = ItemId(UUID.randomUUID())
    private val result = for (
      invoice1 <- invoice;
      itemId1 <- itemId;
      invoice2 <- invoice1.delete(Seq(itemId1))
    ) yield invoice2

    assert(result.isBad)
  }

  it can "be canceled" in new InvoiceFixture {
    private val result = for (
      invoice1 <- invoice;
      invoice2 <- invoice1.cancel()
    ) yield invoice2

    assert(result.isGood)
  }

  it should "be canceled once" in new  InvoiceFixture {
    private val result = for (
      invoice1 <- invoice;
      invoice2 <- invoice1.cancel();
      invoice3 <- invoice2.cancel()
    ) yield invoice3

    assert(result.isBad)
  }

  it should "not allow adding items when canceled" in new InvoiceFixture {
    private val item = lineItemSeq.flatMap(_ (4).copy(quantity = 10))
    private val anotherLineItemSeq = item.map(Seq(_))
    private val result: Invoice Or Every[Error] = for (
      invoice1 <- invoice;
      invoice2 <- invoice1.cancel();
      lineItemSeq1 <- anotherLineItemSeq;
      invoice3 <- invoice2.add(lineItemSeq1)
    ) yield invoice3

    assert(result.isBad)
  }

  it should "not allow deleting items when canceled" in new InvoiceFixture {
    private val itemIds = lineItemSeq.map(l => Seq(l(5).item.id, l(8).item.id))
    private val result = for (
      invoice1 <- invoice;
      invoice2 <- invoice1.cancel();
      itemIds1 <- itemIds;
      invoice3 <- invoice2.delete(itemIds1)
    ) yield invoice3

    assert(result.isBad)
  }

  it can "be issued" in new InvoiceFixture {
    private val result = for (
      invoice1 <- invoice;
      invoice2 <- invoice1.issue()
    ) yield invoice2

    assert(result.isGood)
  }

  it should "be issued once" in new InvoiceFixture {
    private val result = for (
      invoice1 <- invoice;
      invoice2 <- invoice1.issue();
      invoice3 <- invoice2.issue()
    ) yield invoice3

    assert(result.isBad)
  }

  it should "have a least one line item when being issued" in new InvoiceFixture {
    private val result = for (
      invoice1 <- emptyInvoice;
      invoice2 <- invoice1.issue()
    ) yield invoice2

    assert(result.isBad)
  }

  it should "not allow adding items after being issued" in new InvoiceFixture {
    private val item = lineItemSeq.flatMap(_ (4).copy(quantity = 10))
    private val anotherLineItemSeq = item.map(Seq(_))
    private val result: Invoice Or Every[Error] = for (
      invoice1 <- invoice;
      invoice2 <- invoice1.issue();
      lineItemSeq1 <- anotherLineItemSeq;
      invoice3 <- invoice2.add(lineItemSeq1)
    ) yield invoice3

    assert(result.isBad)
  }

  it should "not allow deleting items after being issued" in new InvoiceFixture {
    private val itemIds = lineItemSeq.map(l => Seq(l(5).item.id, l(8).item.id))
    private val result = for (
      invoice1 <- invoice;
      invoice2 <- invoice1.issue();
      itemIds1 <- itemIds;
      invoice3 <- invoice2.delete(itemIds1)
    ) yield invoice3

    assert(result.isBad)
  }

  it can "be cancel after being issued" in new InvoiceFixture {
    private val result = for (
      invoice1 <- invoice;
      invoice2 <- invoice1.issue();
      invoice3 <- invoice2.cancel()
    ) yield invoice3

    assert(result.isGood)
  }

}
