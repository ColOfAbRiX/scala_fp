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

  def sum3( ns: List[Int] ): Int = foldLeft( ns, 0 )( _ + _ )

  def product3( ds: List[Double] ): Double = foldLeft( ds, 1.0 )( _ * _ )

  def length2[A]( as: List[A] ): Int = foldLeft( as, 0 )( ( _, c ) => c + 1 )

  // --- Excercise 3.12 --- //

  // Tail-recursive
  def reverse[A]( as: List[A] ): List[A] = {
    @tailrec
    def loop( as: List[A], acc: List[A] ): List[A] = as match {
      case Nil => acc
      case Cons( x, xs ) => loop( xs, Cons(x, acc) )
    }

    loop( as, Nil )
  }

  // Using foldLeft
  def reverse2[A]( as: List[A] ): List[A] = foldLeft( as, Nil: List[A] )( (x, xs) => Cons(x, xs) )

  // --- Excercise 3.13 --- //

  // It works but it's probably cheating and also less efficient
  def foldLeft2[A, B]( as: List[A], z: B )( f: (A, B) => B ): B = foldRight( List.reverse(as), z )(f)
  def foldRight2[A, B]( as: List[A], z: B )( f: (A, B) => B ): B = foldLeft( List.reverse(as), z )(f)

  // Solution found online - http://stackoverflow.com/questions/17136794/foldleft-using-foldright-in-scala
  def foldLeft3[A, B]( as: List[A], z: B )( f: (B, A) => B ): B =
    foldRight( as, (s: B) => z )( (a, g) => b => g(f(b, a)) )(z)
}
