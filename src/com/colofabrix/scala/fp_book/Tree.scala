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

  /* --- Excercise 3.27 ---
   * Write a function depth that returns the maximum path length from the root of a tree to any leaf.
   */
  def depth[T]( t: Tree[T] ): Int = t match {
    case l: Leaf[T] => 1
    case b: Branch[T] => 1 + Math.max( depth(b.left), depth(b.right) )
  }

  /* --- Excercise 3.28 ---
   * Write a function map, analogous to the method of the same name on List, that modifies each element in a tree
   * with a given function
   */
  def map[T, U]( t: Tree[T] )( f: T => U ): Tree[U] = t match {
    case l: Leaf[T] => Leaf( f( l.value ) )
    case b: Branch[T] => Branch[U]( map(b.left)(f), map(b.right)(f) )
  }
}