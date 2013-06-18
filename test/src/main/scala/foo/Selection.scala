package foo

import Fitness._
import scala.util.Random

case class Roulette(size: SelectionSize = Percentage(33)) extends Selection {
  def apply(pop: GenomeVal, rnd: util.Random): Genome = ???
}

case class Truncation(size: SelectionSize = Percentage()) extends Selection {
  def apply(pop: GenomeVal, rnd: Random): Genome = ???
}

sealed trait Selection extends ((GenomeVal, util.Random) => Genome)

case class Number(value: Int = 10) extends SelectionSize {
  def apply(pop: Int): Int = ???
}

case class Percentage(value: Int = 20) extends SelectionSize {
  def apply(pop: Int): Int = ???
}

sealed trait SelectionSize extends (Int => Int)