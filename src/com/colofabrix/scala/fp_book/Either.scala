package com.colofabrix.scala.fp_book

sealed trait Either[+E, +A] {

  /* --- Exercise 4.6 ---
   * Implement versions of map, flatMap, orElse and map2 on Either that operate on the Right value
   */
  def map[B]( f: A => B ): Either[E, B] = this match {
    case Left( value ) => Left( value )
    case Right( value ) => Right( f( value ) )
  }

  def flatMap[EE >: E, B]( f: A => Either[EE, B] ): Either[EE, B] = this match {
    case Left( value ) => Left( value )
    case Right( value ) => f( value )
  }

  def orElse[EE >: E, B >: A]( b: => Either[EE, B] ): Either[EE, B] = this match {
    case Left( _ ) => b
    case Right( value ) => Right( value )
  }

  def map2[EE >: E, B, C]( b: Either[EE, B] )( f: (A, B) => C ): Either[EE, C] = this match {
    case Left( value ) => Left( value )
    case Right( value ) => b match {
      case Left( bValue ) => Left( bValue )
      case Right( bValue ) => Right( f( value, bValue ) )
    }
  }
}

object Either {

  import scala.{ List => SList, Nil => SNil }

  /* --- Exercise 4.7 ---
   * Implement sequence and traverse for Either. These should return the first error that's
   * encountered, if there is one
   */

  def sequence[E, A]( es: SList[Either[E, A]] ): Either[E, SList[A]] = es match {
    case x :: xs => x match {
      case Left( v ) => Left( v )
      case Right( v ) => sequence( xs ).map2( x )( _.::( _ ) )
    }
    case SNil => Right( SNil )
  }

  def traverse[E, A, B]( as: SList[A] )( f: A => Either[E, B] ): Either[E, SList[B]] = as match {
    case x :: xs => traverse( xs )( f ).map2( f( x ) )( _.::( _ ) )
    case SNil => Right( SNil )
  }

}

case class Left[+E]( value: E ) extends Either[E, Nothing]

case class Right[A]( value: A ) extends Either[Nothing, A]