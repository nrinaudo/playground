package separated

// - Seed handling -----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
opaque type Seed = Long

extension (seed: Seed)
  /** Returns the next seed in the semi-random series. Honestly don't remember where I got the algorithm from, and it
    * is, for all I know, terrible. Plugging in something better shouldn't be very hard, but this is toy code, not
    * something to be used seriously.
    */
  def next: Seed                  = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
  def int: Int                    = (seed >> 16).toInt
  def imap(f: Long => Long): Seed = f(seed)

case class GenValue[A](seed: Seed, value: A):
  def map[B](f: A => B): GenValue[B] = copy(value = f(value))

trait Gen[A]:
  def gen(seed: Seed): GenValue[A]

  def map[B](f: A => B): Gen[B] = seed => gen(seed).map(f)

  def flatMap[B](f: A => Gen[B]): Gen[B] = seed =>
    val GenValue(newSeed, a) = gen(seed)
    f(a).gen(newSeed)

object Gen:
  val int: Gen[Int] = seed =>
    val next = seed.next
    GenValue(next, next.int)

  def range(min: Int, max: Int): Gen[Int] = int.map { value =>
    val intervalLength = max.toLong - min.toLong
    val totalLength    = (Int.MaxValue.toLong - Int.MinValue.toLong).toDouble

    ((value - Int.MinValue.toLong) / totalLength * intervalLength + min).toInt
  }

  def const[A](value: A): Gen[A] = seed => GenValue(seed, value)

  def listOf[A](genSize: Gen[Int], gen: Gen[A]): Gen[List[A]] =
    genSize.flatMap(size => listOfN(size, gen))

  def listOfN[A](n: Int, gen: Gen[A]): Gen[List[A]] =
    if n <= 0 then const(Nil)
    else
      for
        head <- gen
        tail <- listOfN(n - 1, gen)
      yield head :: tail

  def function[A: CoGen, B](genB: Gen[B]): Gen[A => B] = seed =>
    // This ensures that we're always:
    // - returning the same B for a given A (`seed` is fixed).
    // - do not necessarily return the same B for two different As (`seed` is modified by A).
    val f: A => B = a =>
      val perturbed = a.perturb(seed)
      genB.gen(perturbed).value

    GenValue(seed.next, f)

trait CoGen[A]:
  extension (value: A) def perturb(seed: Seed): Seed

object CoGen:
  given CoGen[Int] with
    extension (value: Int) def perturb(seed: Seed): Seed = seed.imap(_ + value.toLong)
