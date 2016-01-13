/**
 * Functional Programming in Scala
 * P. Chiusano, R. Bjarnason
 * Manning Edition
 *
 * Exercises solved by Fabrizio Colonna
 */

import com.colofabrix.scala.fp_book._

object Main {

  def main( argv: Array[String] ): Unit = {
    val list1 = Cons( 4, Cons( 3, Cons( 2, Cons( 1, Nil ) ) ) )
    val list2 = Cons( 1, Nil )
    val list3 = Nil

    val ex32 = List.tail( list1 )
    println( "Ex. 3.2: " + List.toString( ex32 ) )

    val ex33 = List.setHead( list1, 5 )
    println( "Ex. 3.3: " + List.toString( ex33 ) )

    val ex34 = List.drop( list1, 2 )
    println( "Ex. 3.4: " + List.toString( ex34 ) )

    val ex35 = List.dropWhile( list1, (x: Int) => x > 2 )
    println( "Ex. 3.5: " + List.toString( ex35 ) )

    val ex36 = List.init( list1 )
    println( "Ex. 3.6: " + List.toString( ex36 ) )

    val ex39 = List.length( list1 )
    println( "Ex. 3.9: " + ex39 )

    val ex310 = List.foldLeft( list1, 0 )( _ + _ )
    println( "Ex. 3.10: " + ex310 )

    val ex311 = List.sum3( list1 )
    println( "Ex. 3.11: " + ex311 )

    val ex312 = List.reverse( list1 )
    println( "Ex. 3.12: " + List.toString( list1 ) )
    println( "Ex. 3.12: " + List.toString( ex312 ) )
  }

}
