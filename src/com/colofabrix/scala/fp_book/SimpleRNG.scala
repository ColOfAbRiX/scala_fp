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

  def toStream( f: RNG => (Int, RNG) ): Stream[Int] =  Stream.unfold( this: RNG ) { s => Some( f( s ) ) }
}

object SimpleRNG {

  /* --- Exercise 6.1 --- *
   * Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue (inclusive).
   * Make sure to handle the corner case when nextInt returns Int.MinValue, which doesn't have a non-negaive
   * counterpart.
   */
  def nonNegativeInt( rng: RNG ): (Int, RNG) = {
    val (n, nextRng) = rng.nextInt
    (if( n == Int.MinValue ) 0 else Math.abs( n ), nextRng)
  }

}