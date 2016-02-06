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

  /* --- Exercise 3.20 ---
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
    flatMap { x => if( f( x ) ) Some( x ) else None }

  /* --- Exercise 4.3 ---
   * Write a generic function map2 that combines two Option values using a binary function. If
   * either Option value is None, then the return value is too.
   */
  def map2[B, C]( b: Option[B] )( f: (A, B) => C ): Option[C] =
    this flatMap { ax =>
      b flatMap { bx =>
        Some( f( ax, bx ) )
      }
    }
}

case class Some[+A]( get: A ) extends Option[A]

case object None extends Option[Nothing]

object Option {

  /* --- Exercise 4.2 ---
   * Implement the variance function in terms of flatMap.
   */
  def variance( xs: Seq[Double] ): Option[Double] = {
    val xMean = mean( xs )
    xMean.flatMap { m =>
      mean(
        xs.map { x =>
          Math.pow( x - m, 2 )
        }
      )
    }
  }

  def mean( xs: Seq[Double] ): Option[Double] = if( xs.isEmpty ) None else Some( xs.sum / xs.length )

  /* --- Exercise 4.4 ---
   * Write a function sequence that combines a list of Options into one Option containing a list
   * of all the Some values in the original list. If the original list contains None even once,
   * the result of the function should be None; otherwise the result should be Some with a list of
   * all the values
   */
  def sequence[A]( a: List[Option[A]] ): Option[List[A]] = a match {
    case Nil => None
    case Cons( x, Nil ) => x.map( Cons( _, Nil ) )
    case Cons( x, xs ) => sequence( xs ).map2( x )( ( ys, y ) => Cons( y, ys ) )
  }

  /* --- Exercise 4.5 ---
   * Implement the traverse function. It's straightforward to do using map and sequence, but try
   * for a more efficient implementation that only looks at the list once. In fact, implement
   * sequence in terms of traverse
   */
  def traverse[A, B]( a: List[A] )( f: A => Option[B] ): Option[List[B]] = a match {
    case Nil => None
    case Cons( x, Nil ) => f( x ).map( Cons( _, Nil ) )
    case Cons( x, xs ) => traverse( xs )( f ).map2( f( x ) )( ( ys, y ) => Cons( y, ys ) )
  }

  def sequence2[A]( a: List[Option[A]] ) = traverse( a )( x => x )

}