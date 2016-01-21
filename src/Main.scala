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
    val list4 = Cons( 4.1, Cons( 3.2, Cons( 2.3, Cons( 1.4, Nil ) ) ) )
    val list5 = List( 4, 3 )

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

    val ex322 = List.addTogether( list1, list2 )
    println( "Ex. 3.22: " + List.toString(ex322) )

    val ex323 = List.zipWith( list1, list4 )( Math.pow(_, _).toString )
    println( "Ex. 3.23: " + List.toString(ex323) )

    val ex324 = List.hasSubsequence( list1, list5 )
    println( "Ex. 3.24: " + ex324 )

    val tree1: Tree[Int] = new Branch[Int](
      new Leaf(5),
      new Branch[Int](
        new Leaf(12),
        new Leaf(-5)
      )
    )

    val ex325 = Tree.count( tree1 )
    println( "Ex. 3.25: " + ex325 )

    val ex326 = Tree.max( tree1 )
    println( "Ex. 3.26: " + ex326 )

    val ex327 = Tree.depth( tree1 )
    println( "Ex. 3.27: " + ex327 )

    val ex328 = Tree.map[Int, Double]( tree1 )( x => Math.sqrt(x.toDouble) )
    println( "Ex. 3.28: " + ex328 )

    val ex329 = Tree.max2( tree1 )
    val ex329b = Tree.map2[Int, Double]( tree1 )( x => Math.sqrt(x.toDouble) )
    println( "Ex. 3.29: " + ex329 )
    println( "Ex. 3.29b: " + ex329b )
  }

}
