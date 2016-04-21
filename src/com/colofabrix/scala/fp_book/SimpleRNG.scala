package com.colofabrix.scala.fp_book

// --- Listing 6.2 --- //

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG( seed: Long ) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG( newSeed )
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  def toStream[A]( z: RNG )( f: Rand[A] ): Stream[A] = Stream.unfold( z ) { s => Some( f( s ) ) }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A]( a: A ): Rand[A] = rng => (a, rng)

  def map[A, B]( s: Rand[A] )( f: A => B ): Rand[B] =
    rng => {
      val (a, rng2) = s( rng )
      (f( a ), rng2)
    }

  /* --- Exercise 6.1 --- *
   * Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue (inclusive).
   * Make sure to handle the corner case when nextInt returns Int.MinValue, which doesn't have a non-negaive
   * counterpart.
   */
  def nonNegativeInt: Rand[Int] = { rng =>
    val (n, nextRng) = rng.nextInt
    (if( n == Int.MinValue ) 0 else Math.abs( n ), nextRng)
  }

  /* --- Exercise 6.2 --- *
   * Write a function to generate a Double between 0 and 1, not including 1. Note: you can use Int.MaxValue
   * to obtain the maximum positive integer value, and you can use x.toDouble to convert an x: Int to a Double
   */
  def double: Rand[Double] = { rng =>
    val (n, nextRng) = nonNegativeInt( rng )
    (n.toDouble / Int.MaxValue, nextRng)
  }

  /* --- Exercise 6.3 --- *
   * Write functions to generate an (Int, Double) pair, a (Double, Int) pair and a (Double, Double, Double) 3-tuple.
   * You should be able to reuse the functions you've already written
   */
  def intDouble: Rand[(Int, Double)] = { rng =>
    val (int1, nextRng1) = nonNegativeInt( rng )
    val (double1, nextRng2) = double( nextRng1 )
    ((int1, double1), nextRng2)
  }

  def doubleInt: Rand[(Double, Int)] = { rng =>
    val (double1, nextRng1) = double( rng )
    val (int1, nextRng2) = nonNegativeInt( nextRng1 )
    ((double1, int1), nextRng2)
  }

  def double3: Rand[(Double, Double, Double)] = { rng =>
    val (double1, nextRng1) = double( rng )
    val (double2, nextRng2) = double( nextRng1 )
    val (double3, nextRng3) = double( nextRng2 )
    ((double1, double2, double3), nextRng3)
  }

  /* --- Exercise 6.4 --- *
   * Write a function to generate a list of random integers
   */
  def ints( count: Int ): Rand[Seq[Int]] = { rng =>
    (0 until count).foldLeft( (Seq.empty[Int], rng) ) { ( a, _ ) =>
      val (n, nextRng) = a._2.nextInt
      (n +: a._1, nextRng)
    }
  }
}