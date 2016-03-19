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
    case Empty => Empty
    case SCons( h, t ) if n > 0 => Stream.cons( h( ), t( ).take( n - 1 ) )
    case SCons( h, _ ) if n <= 0 => SCons( h, ( ) => Empty )
  }

  def drop( n: Int ): Stream[A] = this match {
    case Empty => Empty
    case SCons( _, t ) if n > 0 => t( ).drop( n - 1 )
    case SCons( _, t ) if n <= 0 => t( )
  }

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