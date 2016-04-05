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
  def map2[B]( f: A => B ): Stream[B] =
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

  /* --- Exercise 5.13 ---
   * Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3) and zipAll. The zipAll function
   * should continue the traversal as long as either stream has more elements - it uses Option to indicate
   * whether each stream has been exhausted
   */
  def map[B]( f: A => B ): Stream[B] = Stream.unfold( this ) {
    case Empty => None
    case SCons( a, t ) => Some( (f( a( ) ), t( )) )
  }

  def take2( n: Int ): Stream[A] = Stream.unfold( (this, 0) ) { s =>
    s._1 match {
      case SCons( a, t ) if s._2 < n => Some( (a( ), (t( ), s._2 + 1)) )
      case _ => None
    }
  }

  def takeWhile3( p: A => Boolean ): Stream[A] = Stream.unfold( this ) {
    case SCons( a, t ) if p( a( ) ) => Some( a( ), t( ) )
    case _ => None
  }

  def zipWith[B, C]( bs: Stream[B] )( f: (A, B) => C ): Stream[C] =
    Stream.unfold( (this, bs) ) {
      case (SCons( a, at ), SCons( b, bt )) => Some( (f( a( ), b( ) ), (at( ), bt( ))) )
      case _ => None
    }

  def zipAll[B]( bs: Stream[B] ): Stream[(Option[A], Option[B])] =
    Stream.unfold( (this, bs) ) {
      case (SCons( a, at ), SCons( b, bt )) => Some(
        Tuple2( Some( a( ) ), Some( b( ) ) ),
        Tuple2( at( ), bt( ) )
      )
      case (Empty, SCons( b, bt )) => Some(
        Tuple2( None, Some( b( ) ) ),
        Tuple2( Stream.empty[A], bt( ) )
      )
      case (SCons( a, at ), Empty) => Some(
        Tuple2( Some( a( ) ), None ),
        Tuple2( at( ), Stream.empty[B] )
      )
      case _ => None
    }

  /* --- Exercise 5.14 ---
   * Implement startWith using functions you've already written. It should check if one Stream is a prefix of another.
   * For instance, Stream(1, 2, 3) startWith Stream(1, 2) would be true
   */
  def startWith[B >: A]( s: Stream[B] ): Boolean = this.zipWith( s )( _ == _ ).foldRight( true )( _ && _ )

  /* --- Exercise 5.15 ---
   * Implement tails using unfold. For a given Stream, tails returns the Stream of suffixes of the input sequence
   * starting with the original Stream. For example, given Stream(1, 2, 3), it would return Stream( Stream(1, 2, 3),
   * Stream(2, 3), Stream(3), Stream() )
   */
  def tails: Stream[Stream[A]] = Stream.unfold( this ) {
    case s@SCons( a, t ) => Some( s, t( ) )
    case Empty => None
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
      x.foldRight( acc1 ) { ( y, acc2 ) => Stream.scons( y, acc2 ) }
    }

  /* --- Exercise 5.8 ---
   * Generalize ones slightly to the function constant, which returns an infinite Stream of a given value
   */
  def constant[A]( a: A ): Stream[A] = Stream.scons( a, constant( a ) )

  /* --- Exercise 5.9 ---
   * Write a function that generates an infinite stream of integer, starting from , then n + 1, n + 2 and so on.
   */
  def from( n: Int ): Stream[Int] = Stream.scons( n, from( n + 1 ) )

  /* --- Exercise 5.10 ---
   * Write a function fibs that generates the infinite stream of Fibonacci numbers
   */
  def fibs: Stream[Int] = {
    def loop( n: Int, m: Int ): Stream[Int] = Stream.scons( n, loop( m, n + m ) )
    loop( 0, 1 )
  }

  /* --- Exercise 5.11 ---
   * Write a more general stream-building function called unfold. It takes an initial state and a function
   * for producing both the next state and the next value in the generated stream.
   * Option is used ti indicate when the Stream should be terminated, if at all.
   */
  def unfold[A, S]( z: S )( f: S => Option[(A, S)] ): Stream[A] = {
    def loop( f: S => Option[(A, S)], s: S ): Stream[A] = {
      f( s ) match {
        case None => Stream.empty[A]
        case Some( (a, ns) ) => Stream.scons( a, loop( f, ns ) )
      }
    }

    loop( f, z )
  }

  /* --- Exercise 5.12 ---
   * Write fibs, from, constant and ones in therm of unfold
   */
  def constant2[A]( a: A ): Stream[A] = unfold( a )( s => Some( (s, s) ) )

  def from2( n: Int ): Stream[Int] = unfold( n ) { s => Some( (s, s + 1) ) }

  def fibs2: Stream[Int] = unfold( (0, 1) ) { s =>
    Some( Tuple2( s._1, Tuple2( s._2, s._1 + s._2 ) ) )
  }

}