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
package mcherri.erp.akka.cqrs.es.utils

import org.scalactic.{Bad, Every, Good, Or}

import scala.collection.immutable.Seq

object RichOr {

  /*
   * The code below basically converst from Seq[G Or Every[T]] to Seq[G] Or Every[T].
   * It can be implemented in many ways. The choice here is to first try to accumulate
   * Good values. Once a Bad value is found, the whole result is converted to Bad and
   * all the errors found so far are added to it.
   *
   * TODO: I think this code should reside in scalactic itself. Maybe I should do a PR for them.
   * TODO: This code assume we have a Seq[Or[_]]. It should be generalized to Traversable[Or[_]].
   */
  implicit class SeqHelper[+G, +T](val seq: Seq[G Or Every[T]]) extends AnyVal {
    def sequence(): Seq[G] Or Every[T] = seq.foldLeft(Good(Seq[G]()): Seq[G] Or Every[T]) {
      case (acc, Good(lineItem)) =>
        acc.map(_ :+ lineItem)
      case (acc, Bad(error: Every[T])) =>
        acc match {
          case Good(_) => Bad(error)
          case Bad(every: Every[T]) => Bad(every ++ error)
        }
    }

  }

}
