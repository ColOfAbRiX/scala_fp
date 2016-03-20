/**
  * Functional Programming in Scala
  * P. Chiusano, R. Bjarnason
  * Manning Edition
  *
  * Exercises solved by Fabrizio Colonna
  *
  * Chapter 5
  */

package com.colofabrix.scala.fp_book

// --- Listing 4.2 --- //

sealed trait Stream[+A] {

  override def toString: String = this match {
    case Empty => ""
    case SCons( h, t ) => h( ).toString + " " + t( ).toString
  }

  def exists( p: A => Boolean ): Boolean = this match {
    case SCons( h, t ) => p( h( ) ) || t( ).exists( p )
    case Empty => false
  }

  def foldRight[B]( z: => B )( f: (A, => B) => B ): B =
    this match {
      case SCons( h, t ) => f( h( ), t( ).foldRight( z )( f ) )
      case _ => z
    }

  /* --- Exercise 5.1 ---
   * Write a function to convert a Stream toa List, which will force its evaluation
   * and let you look at in the REPL. You can convert the regular List type in the
   * standard library. You can place this and other functions that operate on a
   * Stream inside the Stream trait
   */
  def toList: scala.List[A] = this match {
    case Empty => scala.Nil
    case SCons( h, t ) => h( ) :: t( ).toList
  }

  /* --- Exercise 5.2 ---
   * Write the function take(n) for returning the first n elements of a Stream, and
   * drop(n) for skipping the first n elements of a Stream
   */
  def take( n: Int ): Stream[A] = this match {
    case SCons( h, t ) if n > 0 => Stream.cons( h( ), t( ).take( n - 1 ) )
    case _ => Empty
  }

  def drop( n: Int ): Stream[A] = this match {
    case Empty => Empty
    case SCons( _, t ) if n > 1 => t( ).drop( n - 1 )
    case SCons( _, t ) if n <= 1 => t( )
  }

  /* --- Exercise 5.3---
   * Write the function takeWhile for returning all the starting elements of a Stream that
   * match the given predicate
   */
  def takeWhile( p: A => Boolean ): Stream[A] = this match {
    case Empty => Empty
    case SCons( h, t ) if p( h( ) ) => SCons( h, ( ) => t( ).takeWhile( p ) )
    case SCons( h, t ) => Empty
  }

  /* --- Exercise 5.4---
   * Implement forAll, which checks that all elements in the Stream match a given predicate.
   * Your implementation should terminate the trasversal as soon as it encounters a nonmatchiing
   * value.
   */
  def forAll( p: A => Boolean ): Boolean = foldRight( true )( ( b, a ) => a && p( b ) )

}

case object Empty extends Stream[Nothing]

case class SCons[+A]( h: () => A, t: () => Stream[A] ) extends Stream[A]

object Stream {
  def cons[A]( hd: => A, tl: => Stream[A] ): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    SCons( ( ) => head, ( ) => tail )
  }

  def empty[A]: Stream[A] = Empty

  def apply[A]( as: A* ): Stream[A] =
    if( as.isEmpty ) empty else cons( as.head, apply( as.tail: _* ) )
}