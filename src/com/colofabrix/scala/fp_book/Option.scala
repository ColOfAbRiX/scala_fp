/**
  * Functional Programming in Scala
  * P. Chiusano, R. Bjarnason
  * Manning Edition
  *
  * Exercises solved by Fabrizio Colonna
  *
  * Chapter 4
  */

package com.colofabrix.scala.fp_book

sealed trait Option[+A] {

  // --- Listing 4.2 --- //

  /* --- Exercise 3.20 --
   * Implement all the function map, flatMap, getOrElse, orElse and filter on Option. It's fine to use
   * pattern matching, though you should be able to implement all the function besides map and getOrElse
   * without resorting to pattern matching.
   */

  // Apply f if Option is not None
  def map[B]( f: A => B ): Option[B] = flatMap( x => Some( f( x ) ) )

  // Apply f, which may fail, to the Option if not None
  def flatMap[B]( f: A => Option[B] ): Option[B] = this match {
    case None => None
    case Some( x ) => f( x )
  }

  // Returns the result inside the Some case of the Option, or, if the Option is None, returns the
  // default given value
  def getOrElse[B >: A]( default: => B ): B = this match {
    case None => default
    case Some( x ) => x
  }

  // Returns the first Option if it's defined, otherwise it returns the second Option
  def orElse[B >: A]( ob: => Option[B] ): Option[B] = flatMap( x => ob )

  // Convert Some to None if the value doesn't satisfy f
  def filter( f: A => Boolean ): Option[A] =
    flatMap { x => if( f(x) ) Some(x) else None }
}

case class Some[+A]( get: A ) extends Option[A]

case object None extends Option[Nothing]

object OptionSupport {

  /* --- Exercise 3.20 --
   * Implement the variance function in terms of flatMap.
   */
  def variance( xs: Seq[Double] ): Option[Double] = {
    val xMean = mean( xs )
    xMean.flatMap { m =>
      mean( xs.map { x =>
        Math.pow( x - m, 2 )
      } )
    }
  }

  def mean( xs: Seq[Double] ): Option[Double] = if( xs.isEmpty ) None else Some( xs.sum / xs.length )

}