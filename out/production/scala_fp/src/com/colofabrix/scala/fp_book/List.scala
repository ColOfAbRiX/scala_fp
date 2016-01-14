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
    case Nil => " "
    case Cons( x, xs ) => x.toString + " " + toString( xs )
  }

  // --- Listing 3.2 --- //

  def foldRight[A, B]( as: List[A], z: B )( f: (A, B) => B ): B = as match {
    case Nil => z
    case Cons( x, xs ) => f( x, foldRight( xs, z )( f ) )
  }

  def sum2( ns: List[Int] ) = foldRight( ns, 0 )( ( x, y ) => x + y )

  def product2( ns: List[Double] ) = foldRight( ns, 1.0 )( _ * _ )

  /* --- Excercise 3.2 --- *
   * Implement the function tail for removing the first element of a List. Note that the function takes constant time.
   * What are the different choices you could make in your implementation if the List is Nil?
   */
  def tail[A]( as: List[A] ) = as match {
    case Nil => Nil
    case Cons( h, t ) => t
  }

  /* --- Excercise 3.3 --- *
   * [...] implement the function setHead for replacing the first element of a List with a different value
   */
  def setHead[A]( as: List[A], h: A ): List[A] = as match {
    case Nil => Nil
    case Cons( x, xs ) => Cons( h, xs )
  }

  /* --- Excercise 3.4 --- *
   * Generalise tail to the function drop, which removes the first n elements from a list. Note that this function
   * takes time proportional only to the number of elements being dropped.
   */
  def drop[A]( as: List[A], n: Int ): List[A] = as match {
    case Cons( x, xs ) if n > 0 => drop( xs, n - 1 )
    case _ => as
  }

  /* --- Excercise 3.5 ---
   * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
   */
  def dropWhile[A]( as: List[A], f: A => Boolean ): List[A] = as match {
    case Cons( x, xs ) if f( x ) => dropWhile( xs, f )
    case _ => as
  }

  /* --- Excercise 3.6 ---
   * Implement a function, init, that returns a List consisting of all but the last element of a List.
   */
  def init[A]( as: List[A] ): List[A] = as match {
    case Nil => Nil
    case Cons( x, Nil ) => Nil
    case Cons( x, xs ) => Cons( x, init( xs ) )
  }

  /* --- Excercise 3.9 ---
   * Compite the length of a list using foldRight
   */
  def length[A]( as: List[A] ): Int = foldRight( as, 0 )( ( _, c ) => c + 1 )

  /* --- Excercise 3.10 ---
   * [...] write another general list-recursion function, foldLeft, that is tail-recursive.
   */
  @tailrec
  def foldLeft[A, B]( as: List[A], z: B )( f: (A, B) => B ): B = as match {
    case Nil => z
    case Cons( x, xs ) => foldLeft( xs, f(x, z) )(f)
  }

  /* --- Excercise 3.11 ---
   * Write sum, production and a function to compute the length of a list using foldLeft.
   */

  def sum3( ns: List[Int] ): Int = foldLeft( ns, 0 )( _ + _ )

  def product3( ds: List[Double] ): Double = foldLeft( ds, 1.0 )( _ * _ )

  def length2[A]( as: List[A] ): Int = foldLeft( as, 0 )( ( _, c ) => c + 1 )

  /* --- Excercise 3.12 ---
   * Write a function .that returns the reverse o fa list
   */

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

  /* --- Excercise 3.13 ---
   * HARD: Can you write foldLeft in terms of foldRight? How about the other way around?
   */

  // It works but it's probably cheating and also less efficient
  def foldLeft2[A, B]( as: List[A], z: B )( f: (A, B) => B ): B = foldRight( List.reverse(as), z )(f)
  def foldRight2[A, B]( as: List[A], z: B )( f: (A, B) => B ): B = foldLeft( List.reverse(as), z )(f)

  /* --- Excercise 3.14 ---
   * Implement append in temrs of either foldLeft or foldRight
   */
  def append[A]( as: List[A], a: A ): List[A] = foldRight( as, Cons(a, Nil) )( Cons(_: A, _: List[A]) )

  /* --- Excercise 3.15 ---
   * HARD: Write a function that concatenates a list of lists into a single lists. Its runtime should be linear in the
   * total length of all lists. Try to use the functions we have already defined.
   */
  def concatenate[A]( as: List[List[A]] ): List[A] = as match {
    case Nil => Nil
    case Cons( x, xs ) => foldRight( x, concatenate(xs) )( Cons(_, _) )
  }

  /* --- Excercise 3.16 ---
   * Wrote a fimctopm tjat tramsfpr,s a ;ost pf integers by adding 1 to each element.
   */
  def add1( is: List[Int] ): List[Int] = foldLeft( is, Nil: List[Int] )( (x, xs) => Cons( x + 1, xs) )

  /* --- Excercise 3.17 ---
   * Write a function that turns each value in a List[Double] into a String.
   */
  def doubleToString( ds: List[Double] ): List[Double] = foldLeft( ds, Nil: List[Double] )( (x, xs) => Cons( x, xs) )

  /* --- Exercise 3.18 --
   * Write a function that generalizes modifying each element in a list while maintaining the structure pf the list.
   */
  def map[A, B]( as: List[A] )( f: A => B ): List[B] = foldRight( as, Nil: List[B] )( (x, xs) => Cons( f(x), xs ) )

  /* --- Exercise 3.19 --
   * Write a function filter that removes elements from a list unless they satisfy a given predicate. Use it to remove
   * all odd numbers from a List[Int]
   */
  def filter[A]( as: List[A] )( f: A => Boolean ): List[A] = as match {
    case Nil => Nil
    case Cons( x, xs ) => if( f(x) ) Cons( x, filter(xs)(f) ) else filter(xs)(f)
  }

  /* --- Exercise 3.20 --
   * Write a function flatMap that works like map except that the function given will return a list instead of a single
   * result, and that list should be inserted into the final resulting
   */
  def flatMap[A, B]( as: List[A] )( f: A => List[B] ): List[B] = List.concatenate( List.map( as )( f ) )

  /* --- Exercise 3.21 --
   * Use flatMap to implement filter
   */
  def filter2[A]( as: List[A] )( f: A => Boolean ): List[A] =
    List.flatMap( as )( x => if( f(x) ) Cons(x, Nil) else Nil  )

  /* --- Exercise 3.22 --
   * Write a function that accepts two lists and constructs a new list by adding corresponding elements
   */
  def addTogether( as1: List[Int], as2: List[Int] ): List[Int] = as1 match {
    case Nil => Nil
    case Cons( x1, xs1 ) => as2 match {
      case Nil => Nil
      case Cons( x2, xs2 ) => Cons( x1 + x2, addTogether( xs1, xs2 ) )
    }
  }

}
