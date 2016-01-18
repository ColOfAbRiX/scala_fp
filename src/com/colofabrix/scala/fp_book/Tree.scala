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

sealed trait Tree[+A]

case class Leaf[A]( value: A ) extends Tree[A]

case class Branch[A]( left: Tree[A], right: Tree[A] ) extends Tree[A]