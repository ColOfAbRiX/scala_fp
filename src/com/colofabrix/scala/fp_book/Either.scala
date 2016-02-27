package com.colofabrix.scala.fp_book

sealed trait Either[+E, +A]

case class Left[+E] (value: E) extends Either[E, Nothing]

case class Right[A] (value: A) extends Either[Nothing, A]