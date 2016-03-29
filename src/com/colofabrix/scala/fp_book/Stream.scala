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

import scala.annotation.tailrec

// --- Listing 4.2 --- //

sealed trait Stream[+A] {

  override def toString: String = this match {
    case Empty => ""
    case SCons( h, t ) => h( ).toString + " " + t( ).toString
  }

  @tailrec
  final def exists( p: A => Boolean ): Boolean = this match {
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
    case SCons( h, t ) if n > 0 => Stream.scons( h( ), t( ).take( n - 1 ) )
    case _ => Empty
  }

  @tailrec
  final def drop( n: Int ): Stream[A] = this match {
    case SCons( _, t ) if n > 0 => t( ).drop( n - 1 )
    case _ => this
  }

  /* --- Exercise 5.3 ---
   * Write the function takeWhile for returning all the starting elements of a Stream that
   * match the given predicate
   */
  def takeWhile( p: A => Boolean ): Stream[A] = this match {
    case SCons( h, t ) if p( h( ) ) => SCons( h, ( ) => t( ).takeWhile( p ) )
    case _ => Empty
  }

  /* --- Exercise 5.4 ---
   * Implement forAll, which checks that all elements in the Stream match a given predicate.
   * Your implementation should terminate the trasversal as soon as it encounters a nonmatching
   * value.
   */
  def forAll( p: A => Boolean ): Boolean = foldRight( true )( ( b, a ) => a && p( b ) )

  /* --- Exercise 5.5 ---
   * Use foldRight to implement takeWhile
   */
  def takeWhile2( p: A => Boolean ): Stream[A] = foldRight( Stream.empty[A] ) { ( a, s ) =>
    if( p( a ) ) SCons( ( ) => a, ( ) => s ) else s
  }

  /* --- Exercise 5.6 ---
   * Implement headOption using foldRight
   */
  def headOption: Option[A] = foldRight( Option.none[A] ) { ( a, o ) => Some( a ) }

  /* --- Exercise 5.7 ---
   * Implement map, filter, append and flatMap using foldRight. The append method should
   * be non strict in its arguments
   */
  def map[B]( f: A => B ): Stream[B] =
    this.foldRight( Stream.empty[B] ) { ( a, b ) =>
      Stream.scons( f( a ), b )
    }

  def flatMap[B]( f: A => Stream[B] ): Stream[B] = Stream.join( this.map( f ) )

  def filter( p: A => Boolean ): Stream[A] =
    this.foldRight( Stream.empty[A] ) { ( a, b ) =>
      if( p( a ) ) Stream.scons( a, b ) else b
    }

  def append[B >: A]( b: () => B ): Stream[B] =
    this.foldRight( Stream.scons[B]( b( ), Empty ) ) {
      Stream.scons( _, _ )
    }

}

case object Empty extends Stream[Nothing]

case class SCons[+A]( h: () => A, t: () => Stream[A] ) extends Stream[A]

object Stream {
  def scons[A]( hd: => A, tl: => Stream[A] ): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    SCons( ( ) => head, ( ) => tail )
  }

  def empty[A]: Stream[A] = Empty

  def apply[A]( as: A* ): Stream[A] =
    if( as.isEmpty ) empty else scons( as.head, apply( as.tail: _* ) )

  def join[A]( xs: Stream[Stream[A]] ): Stream[A] =
    xs.foldRight( Stream.empty[A] ) { ( x, acc1 ) =>
      x.foldRight( acc1 ) { (y, acc2) => Stream.scons( y, acc2 ) }
    }
}