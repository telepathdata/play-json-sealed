package at.iem.point.sh.sketches.genetic

import scala.util.Random
import collection.immutable.{IndexedSeq => Vec}
import scala.annotation.tailrec

object Fitness {
  val TEST_CORPUS = false // `true` to use test corpus of single note cells, `false` to use Sonja's cells

  trait NoteOrRest
  trait Cell
  trait Rational

  type Sequence   = Vec[NoteOrRest]
  type Chromosome = Vec[Cell]
  type Genome     = Vec[Chromosome]
  type GenomeVal  = Vec[(Chromosome, Double)]
  type GenomeSel  = Vec[(Chromosome, Double, Boolean)]

  /** Whether to print log information (for debugging) during calculation or not. */
  var showLog = false // true

  /** The corpus consists of all cells with all stretching factors applied. */

  val corpus: Vec[Cell] = ???

  /** The normalized corpus is equal to the corpus, but cells are already normalized. */
  val norm  : Vec[Cell] = ???

  def log(what: => String) {
    if (showLog) println(s"<ga> $what")
  }

  /** Creates a new random number generator with a given `seed` value. */
  def rng(seed: Long = System.currentTimeMillis()) = new Random(seed)

  /** Runs the whole genetic algorithm, producing an initial population and running over a given number of iterations.
    *
    * @param duration       target duration of the chromosomes (in whole notes)
    * @param iter           number of iterations
    * @param pop            population size
    * @param fitness        fitness function
    * @param selectAndBreed function for selection and breeding
    * @param rnd            random number generator
    * @return               the output population after then `iter`'th iteration.
    */
  def produce(duration: Rational, iter: Int, pop: Int)
             (fitness: Chromosome => Double, selectAndBreed: GenomeVal => Genome)
             (implicit rnd: Random): GenomeVal = {

    @tailrec def loop(g: Genome, it: Int): Genome = if (it <= 0) g else {
      val g1 = iterate(g, fitness, selectAndBreed)
      loop(g1, it - 1)
    }

    val p0  = Vector.fill(pop)(randomSequence(duration))
    val res = loop(p0, iter)
    weigh(res)(fitness)
  }

  /** Evaluates a genome by zipping it with the applied fitness function.
    *
    * @param pop      the population to evaluate
    * @param fitness  the fitness function
    * @return         the population zipped with the result from the fitness function for each chromosome
    */
  def weigh(pop: Genome)(fitness: Chromosome => Double): GenomeVal = pop.map(seq => seq -> fitness(seq))

  /** Performs one iteration of the whole genetic algorithm.
    *
    * @param pop            the input population
    * @param fitness        the fitness function
    * @param selectAndBreed the selection and breeding function
    * @return               the output population
    */
  def iterate(pop: Genome, fitness: Chromosome => Double, selectAndBreed: GenomeVal => Genome): Genome = {
    val weighted  = weigh(pop)(fitness)
    val res       = selectAndBreed(weighted)
    log(s"iterate(pop = ${pop.idString}) = ${res.idString}")
    res
  }

  /** Composes a selection and breeding function by applying an elitism bypass.
    *
    * @param sz             the size of the elite
    * @param selectAndBreed the function to compose with. this will be called with the population after
    *                       removing the elite
    * @param g              the input population. this method will remove the elite from this set, apply the
    *                       encapsulated `selectAndBreed` function, then re-add the elite to the result
    * @return               the selected and breeded chromosomes, including the diverted elite
    */
  def elitism(sz: Int)(selectAndBreed: GenomeVal => Genome)(g: GenomeVal): Genome = {
    val sorted = g.sortBy(_._2) // highest fitness = last
    val (a, b) = sorted.splitAt(sorted.size - sz)
    selectAndBreed(a) ++ b.drop_2
  }

  /** Selection function which just takes a proportion of the best fitting chromosomes.
    *
    * @param p    the proportion between zero and one
    * @param seq  the genome to select from
    * @return     the selection (truncation) of the best fitting chromosomes
    */
  def truncationSelection(p: Double)(seq: GenomeVal): Genome = {
    val sorted  = seq.sortBy(_._2)
    val sz      = (seq.size * p + 0.5).toInt
    sorted.takeRight(sz).map(_._1)
  }

  //  def truncationSelection(p: Double)(seq: GenomeVal): Genome = {
  //    val sum     = seq.map(_._2).sum
  //    val norm    = 1.0 / sum
  //    val normed  = seq.map { case (c, w) => c -> w * norm }
  //
  //  }

  /**
   * Applies a sliding window to a chromosome, where the window and step size are defined by durations.
   *
   * @param window  the window size as duration (in whole notes). must be greater than or equal to `step`
   * @param step    the step size as duration (in whole notes). must be greater than zero and less than or equal to `window`
   * @param seq     the chromosome to slide across
   * @return  the sliding windows in the order of their succession, annotated with start time and start index.
   */
  def slideByDuration(window: Rational, step: Rational)(seq: Chromosome): Vec[(Rational, Int, Sequence)] = ???
  def slideByEvents(window: Int, step: Int)(seq: Chromosome): Vec[(Rational, Int, Sequence)] = ???

  /**
   * Calculates a sequence of fitness evaluations by applying a sliding window based on duration,
   * and applying a fitness function for each slice.
   *
   * @param window  the window size as duration (in whole notes). must be greater than or equal to `step`
   * @param step    the step size as duration (in whole notes). must be greater than zero and less than or equal to `window`
   * @param fun     the fitness function which is given the sub-sequence and a weighting factor from zero
   *                (first sliding window) to one (last sliding window)
   * @param seq     the chromosome to slide across
   * @return        the sequence of fitnesses thus calculated
   */
  def slidingFitnessByDuration(window: Rational, step: Rational)(fun: (Sequence, Double) => Double)
                              (seq: Chromosome): Vec[Double] = ???

  def slidingFitnessByEvents(window: Int, step: Int)(fun: (Sequence, Double) => Double)
                            (seq: Chromosome): Vec[Double] = ???

  /** Flattens a chromosome to a sequence of notes or rests, and zips it with the running sum of the durations */
  def flatWithAccum(seq: Chromosome): Vec[(NoteOrRest, Rational)] = {
    val flat      = seq.flattenCells
    val zipped    = flat.accumSeqDur
    zipped
  }

  implicit final class RichIndexedSeq[A](val seq: Vec[A]) extends AnyVal {
    /** Chooses a random element of the sequence. */
    def choose()(implicit rnd: Random): A = seq(rnd.nextInt(seq.size))
    /** Converts a sequence of cells to a flat sequence of note-or-rest elements. */
    def flattenCells(implicit ev: A <:< Cell): Sequence = ???
    /** For a sequence of `Tuple2`, drops the second tuple part. */
    def drop_2[B](implicit ev: A <:< (B, _)): Vec[B] = seq.map(_._1)

    //    def toCell(implicit ev: A <:< Rational): Cell = {
    //      val elems = seq.map(Note(_))
    //      val dur   = elems.map(_.dur).sum
    //      Cell(-1, elems, dur)
    //    }

    /** Removes an element at a given index (0 <= index < size), and returns the new sequence. */
    def removeAt(idx: Int): Vec[A] = {
      if (idx < 0 || idx >= seq.size) throw new IndexOutOfBoundsException(idx.toString)
      seq.patch(idx, Vec.empty, 1)
    }

    /** Inserts an element at a given index (0 <= index <= size), and returns the new sequence. */
    def insertAt(idx: Int, elem: A): Vec[A] = {
      if (idx < 0 || idx > seq.size) throw new IndexOutOfBoundsException(idx.toString)
      seq.patch(idx, Vec(elem), 0)
    }

    /** Removes rests by adding their duration to preceeding notes. */
    def bindTrailingRests(implicit ev: A <:< NoteOrRest): Vec[Rational] = ???

    /** Converts a flat sequence of note-or-rest elements to a single cell. */
    def toCell(implicit ev: A <:< NoteOrRest): Cell = ???

    /** Creates a short string representation of a genome, by just referring to the cell ids and their total duration. */
    def idString(implicit ev: A <:< Chromosome): String = ???

    /** Calculates the total duration of a chromosome. */
    def dur(implicit ev: A <:< Cell): Rational = ???

    /** Zips a chromosome with the running sum of its duration. */
    def accumDur/* (beginWithZero: Boolean) */(implicit ev: A <:< Cell): Vec[(A, Rational)] = ???

    /** Zips a note-or-rest sequence with the running sum of its duration. */
    def accumSeqDur/* (beginWithZero: Boolean) */(implicit ev: A <:< NoteOrRest): Vec[(A, Rational)] = ???

    /** Given a sequence which is at least as long as the reference, calculates the relative error
      * for the sequence length with respect to the reference length, and compares it to the relative error
      * which would occur when one more element is dropped from the end.
      *
      * @param ref    reference duration
      * @param view   (running sum) duration view of the sequence
      * @param num    numeric evidence of the duration view
      * @tparam B     type of duration
      * @return       either the input sequence, or the input sequence minus the last element, if that
      *               yields a smaller relative error with respect to the reference duration
      */
    def optimumEnd[B](ref: B)(view: A => B)(implicit num: Fractional[B]): Vec[A] = ???

    /** Given a sequence which is at least as long as the reference, calculates the relative error
      * for the sequence length with respect to the reference length, and compares it to the relative error
      * which would occur when one more element is dropped from the beginning.
      *
      * @param ref    reference duration
      * @param view   (running sum) duration view of the sequence
      * @param num    numeric evidence of the duration view
      * @tparam B     type of duration
      * @return       either the input sequence, or the input sequence minus the first element, if that
      *               yields a smaller relative error with respect to the reference duration
      */
    def optimumStart[B](ref: B)(view: A => B)(implicit num: Fractional[B]): Vec[A] = ???

    // `true` if t1 should be dropped
  }

  /** Given a reference value `ref` and two elements `t1` and `t2` which can be translated to values,
    * calculate the relative errors of the latter with respect to the reference, and indicate which
    * is the element with smaller error.
    *
    * @param ref    the reference value
    * @param t1     the first  alternative element
    * @param t2     the second alternative element
    * @param view   the value view of the elements
    * @param num    a numeric type class to work with the values
    * @return       the element with the smaller relative error
    */
  def optimize[A, B](ref: B, t1: A, t2: A)(view: A => B)(implicit num: Fractional[B]): A = ???

  /** Generates a random sequence from the `corpus` which is at least as long as a given `duration`. */
  def randomSequence(duration: Rational)(implicit rnd: Random): Chromosome = ???

  /** This was a test to find alternative sequence slices by selectively dropping cells, until valid
    * cell boundaries are found. Does not yield a lot of useful results.
    */
  def boundaryVersions(seq: Sequence, drop: Double = 0, durTol: Rational = ???)
                      (implicit rnd: Random): Genome = ???
}