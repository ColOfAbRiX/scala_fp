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

object Tree {

  /* --- Excercise 3.25 ---
   * Write a function that counts the number of nodes (leaves and branches) in a tree
   */
  def count[A]( t: Tree[A] ): Int = t match {
    case l: Leaf[A] => 1
    case b: Branch[A] => 1 + count( b.left ) + count( b.right )
  }

  /* --- Excercise 3.26 ---
   * Write a function maximum that returns the maximum element in a Tree[Int]
   */
  def max( t: Tree[Int] ): Int = t match {
    case l: Leaf[Int] => l.value
    case b: Branch[Int] => Math.max( max( b.left ), max( b.right ) )
  }

}