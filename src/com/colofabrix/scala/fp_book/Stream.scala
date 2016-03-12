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

sealed trait Stream[+A]

case object Empty extends Stream[Nothing]

case class Cons[+A]( h: () => A, t: () => Stream[A] ) extends Stream[A]

object Stream {
  def cons[A]( hd: => A, tl: => Stream[A] ): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons( ( ) => head, ( ) => tail )
  }

  def empty[A]: Stream[A] = Empty

  def apply[A]( as: A* ): Stream[A] =
    if( as.isEmpty ) empty else cons( as.head, apply( as.tail: _* ) )
}