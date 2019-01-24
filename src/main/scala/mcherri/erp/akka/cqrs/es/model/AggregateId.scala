package mcherri.erp.akka.cqrs.es.model

import java.util.UUID

trait AggregateId {

  def value: String

}

trait LongAggregateId extends AggregateId {

  def id: Long

  override final val value: String = id.toString

}

trait UuidAggregateId extends AggregateId {

  def id: UUID

  override final val value: String = id.toString
}