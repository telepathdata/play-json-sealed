package at.iem.point.sh.sketches
package genetic

import collection.immutable.{IndexedSeq => Vec}
import Fitness._
import scala.annotation.tailrec
import scala.util.Random

// object Selection {
  // cf. http://en.wikipedia.org/wiki/Fitness_proportionate_selection
  case class Roulette(size: SelectionSize = /* SelectionSize. */ Percentage(33)) extends Selection {
    override def apply(pop: GenomeVal, rnd: util.Random): Genome = ???
  }

  // cf. http://en.wikipedia.org/wiki/Truncation_selection
  case class Truncation(size: SelectionSize = /* SelectionSize. */ Percentage()) extends Selection {
    override def apply(pop: GenomeVal, rnd: Random): Genome = ???
  }
// }
sealed trait Selection extends ((GenomeVal, util.Random) => Genome)

// object SelectionSize {

  /** Selects an absolute number of individuals
    *
    * @param value  the number of individuals to select
    */
  case class Number(value: Int = 10) extends SelectionSize {
    require(value > 0)
    override def apply(pop: Int): Int = math.min(pop, value)
  }
  /** Selects the number of individuals corresponding to
    * a given percentage of the total population.
    *
    * @param value  the percentage value ranging from 0 to 100
    */
  case class Percentage(value: Int = 20) extends SelectionSize {
    require(value >= 0 && value <= 100)
    override def apply(pop: Int): Int = pop * value / 100
  }
// }
sealed trait SelectionSize extends (Int => Int)