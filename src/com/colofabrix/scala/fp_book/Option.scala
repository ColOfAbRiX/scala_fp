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

  def map[B]( f: A => B ): Option[A]

  def flatMap[B]( f: A => Option[B] ): Option[B]

  def getOrElse[B >: A]( default: => B): B

  def orElse[B >: A]( ob: => Option[B] ): Option[B]

  def filter( f: A => Boolean ): Option[A]

}

case class Some[+A]( get: A ) extends Option[A]

case object None extends Option[Nothing]