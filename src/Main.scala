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
    val list4 = Cons( 4.0, Cons( 3.0, Cons( 2.0, Cons( 1.0, Nil ) ) ) )

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
    println( "Ex. 3.12: " + List.toString( ex312 ) )

    val ex313 = List.foldLeft2( list1, 1.0 )( _ * _ )
    println( "Ex. 3.13: " + ex313 )

    val ex314 = List.append( list1, "10" )
    println( "Ex. 3.14: " + List.toString(ex314) )

    val list315= Cons( list1, Cons(list2, Cons( list1, Nil ) ) )
    val ex315 = List.concatenate( list315 )
    println( "Ex. 3.15: " + List.toString(ex315) )

    val ex316 = List.add1( list1 )
    println( "Ex. 3.16: " + List.toString(ex316) )

    val ex317 = List.doubleToString( list4 )
    println( "Ex. 3.17: " + List.toString(ex317) )

    val ex318 = List.map( list1 )( Math.pow(_, 2).toInt )
    println( "Ex. 3.18: " + List.toString(ex318) )

    val ex319 = List.filter( list1 )( _ % 2 != 0 )
    println( "Ex. 3.19: " + List.toString(ex319) )

    val ex320 = List.flatMap( list1 )( i => List(i, i) )
    println( "Ex. 3.20: " + List.toString(ex320) )

    val ex321 = List.filter2( list1 )( _ % 2 != 0 )
    println( "Ex. 3.21: " + List.toString(ex321) )
  }

}
