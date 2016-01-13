/**
 * Functional Programming in Scala
 * P. Chiusano, R. Bjarnason
 * Manning Edition
 *
 * Exercises solved by Fabrizio Colonna
 *
 * Chapter 3
 */

package com.colofabrix.scala.fp_book

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A]( head: A, tail: List[A] ) extends List[A]

object List {

  // --- Listing 3.1 --- //

  def sum( ints: List[Int] ): Int = ints match {
    case Nil => 0
    case Cons( x, xs ) => x + sum( xs )
  }

  def product( ds: List[Double] ): Double = ds match {
    case Nil => 1.0
    case Cons( 0.0, _ ) => 0.0
    case Cons( x, xs ) => x * product( xs )
  }

  def apply[A]( as: A* ): List[A] = if( as.isEmpty ) Nil else Cons( as.head, apply( as.tail: _* ) )

  def toString[A]( as: List[A] ): String = as match {
    case Nil => ""
    case Cons( x, xs ) => x.toString + toString( xs ) + " "
  }

  // --- Listing 3.2 --- //

  def foldRight[A, B]( as: List[A], z: B )( f: (A, B) => B ): B = as match {
    case Nil => z
    case Cons( x, xs ) => f( x, foldRight( xs, z )( f ) )
  }

  def sum2( ns: List[Int] ) = foldRight( ns, 0 )( ( x, y ) => x + y )

  def product2( ns: List[Double] ) = foldRight( ns, 1.0 )( _ * _ )

  // --- Excercise 3.2 --- //
  def tail[A]( as: List[A] ) = as match {
    case Nil => Nil
    case Cons( h, t ) => t
  }

  // --- Excercise 3.3 --- //
  def setHead[A]( as: List[A], h: A ): List[A] = as match {
    case Nil => Nil
    case Cons( x, xs ) => Cons( h, xs )
  }

  // --- Excercise 3.4 --- //
  def drop[A]( as: List[A], n: Int ): List[A] = as match {
    case Cons( x, xs ) if n > 0 => drop( xs, n - 1 )
    case _ => as
  }

  // --- Excercise 3.5 --- //
  def dropWhile[A]( as: List[A], f: A => Boolean ): List[A] = as match {
    case Cons( x, xs ) if f( x ) => dropWhile( xs, f )
    case _ => as
  }

  // --- Excercise 3.6 --- //
  def init[A]( as: List[A] ): List[A] = as match {
    case Nil => Nil
    case Cons( x, Nil ) => Nil
    case Cons( x, xs ) => Cons( x, init( xs ) )
  }

  // --- Excercise 3.9 --- //
  def length[A]( as: List[A] ): Int = foldRight( as, 0 )( ( _, c ) => c + 1 )

  // --- Excercise 3.10 --- //
  @tailrec
  def foldLeft[A, B]( as: List[A], z: B )( f: (A, B) => B ): B = as match {
    case Nil => z
    case Cons( x, xs ) => foldLeft( xs, f(x, z) )(f)
  }

  // --- Excercise 3.11 --- //

  def sum3( ns: List[Int] ) = foldLeft( ns, 0 )( _ + _ )

  def product3( ds: List[Double] ) = foldLeft( ds, 1.0 )( _ * _ )

  def length2[A]( as: List[A] ): Int = foldLeft( as, 0 )( ( _, c ) => c + 1 )

  // --- Excercise 3.12 --- //
  def reverse[A]( as: List[A] ) = foldLeft( as, Nil: List[A] )( (x, xs) => Cons(x, xs) )
}
