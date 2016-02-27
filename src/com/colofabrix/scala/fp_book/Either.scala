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

case class Left[+E]( value: E ) extends Either[E, Nothing]

case class Right[A]( value: A ) extends Either[Nothing, A]