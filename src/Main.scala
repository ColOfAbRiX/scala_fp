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

    // --- Chapter 3 --- //

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

    val ex35 = List.dropWhile( list1, ( x: Int ) => x > 2 )
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
    println( "Ex. 3.14: " + List.toString( ex314 ) )

    val list315 = Cons( list1, Cons( list2, Cons( list1, Nil ) ) )
    val ex315 = List.concatenate( list315 )
    println( "Ex. 3.15: " + List.toString( ex315 ) )

    val ex316 = List.add1( list1 )
    println( "Ex. 3.16: " + List.toString( ex316 ) )

    val ex317 = List.doubleToString( list4 )
    println( "Ex. 3.17: " + List.toString( ex317 ) )

    val ex318 = List.map( list1 )( Math.pow( _, 2 ).toInt )
    println( "Ex. 3.18: " + List.toString( ex318 ) )

    val ex319 = List.filter( list1 )( _ % 2 != 0 )
    println( "Ex. 3.19: " + List.toString( ex319 ) )

    val ex320 = List.flatMap( list1 )( i => List( i, i ) )
    println( "Ex. 3.20: " + List.toString( ex320 ) )

    val ex321 = List.filter2( list1 )( _ % 2 != 0 )
    println( "Ex. 3.21: " + List.toString( ex321 ) )

    val ex322 = List.addTogether( list1, list2 )
    println( "Ex. 3.22: " + List.toString( ex322 ) )

    val ex323 = List.zipWith( list1, list4 )( Math.pow( _, _ ).toString )
    println( "Ex. 3.23: " + List.toString( ex323 ) )

    val ex324 = List.hasSubsequence( list1, list5 )
    println( "Ex. 3.24: " + ex324 )

    val tree1: Tree[Int] = new Branch[Int](
      new Leaf( 5 ),
      new Branch[Int](
        new Leaf( 12 ),
        new Leaf( -5 )
      )
    )

    val ex325 = Tree.count( tree1 )
    println( "Ex. 3.25: " + ex325 )

    val ex326 = Tree.max( tree1 )
    println( "Ex. 3.26: " + ex326 )

    val ex327 = Tree.depth( tree1 )
    println( "Ex. 3.27: " + ex327 )

    val ex328 = Tree.map[Int, Double]( tree1 )( x => Math.sqrt( x.toDouble ) )
    println( "Ex. 3.28: " + ex328 )

    val ex329 = Tree.max2( tree1 )
    val ex329b = Tree.map2[Int, Double]( tree1 )( x => Math.sqrt( x.toDouble ) )
    println( "Ex. 3.29: " + ex329 )
    println( "Ex. 3.29b: " + ex329b )

    // --- Chapter 4 --- //

    val someIntValue: Option[Int] = Some( 2 )
    val someListValue: List[Option[Int]] = List( Some( 2 ), Some( 1 ), Some( 4 ) )
    val noIntValue: Option[Int] = None

    val ex41a = someIntValue.map( _.toString + "#" )
    println( "Ex. 4.1a: " + ex41a )

    val ex41b = noIntValue.flatMap( x => if( x % 2 == 0 ) None else Some( x ) )
    println( "Ex. 4.1b: " + ex41b )

    val ex42 = Option.variance( Seq( 4.1, 3.2, 2.3, 1.4 ) ).getOrElse( Double.NaN )
    println( "Ex. 4.2b: " + ex42 )

    val ex43 = someIntValue.map2( someIntValue )( _ * _ )
    println( "Ex. 4.3: " + ex43 )

    val ex44 = Option.sequence( someListValue )
    println( "Ex. 4.4: " + ex44 )

    val ex45 = Option.sequence( someListValue )
    println( "Ex. 4.5: " + ex45 )

    val eitherValue1 = Right( 2.0 )
    val eitherValue2 = Left( "Error" )
    val eitherValue3 = Right( 3.0 )

    val ex46 = eitherValue1.map2( eitherValue3 )( Math.pow )
    println( "Ex. 4.6: " + ex46 )

    val ex47a = Either.sequence( scala.List( eitherValue1, eitherValue2, eitherValue3 ) )
    val ex47b = Either.sequence( scala.List( eitherValue1, eitherValue3 ) )
    println( "Ex. 4.7a: " + ex47a )
    println( "Ex. 4.7b: " + ex47b )

    // --- Chapter 6 --- //

    val intStream = Stream( 1, 2, 3, 4, 5 )
    val doubleStream = Stream( 6.0, 5.0, 4.0, 3.0, 2.0, 1.0 )

    val ex51 = intStream.toList
    println( "Ex. 5.1: " + ex51 )

    val ex52a = intStream.take( 2 )
    val ex52b = intStream.drop( 2 )
    println( "Ex. 5.2a: " + ex52a )
    println( "Ex. 5.2b: " + ex52b )

    val ex53 = intStream.takeWhile( _ % 2 != 0 )
    println( "Ex. 5.3: " + ex53 )

    val ex54 = intStream.forAll( _ < 4 )
    println( "Ex. 5.4: " + ex54 )

    val ex55 = intStream.takeWhile( _ % 2 != 0 )
    println( "Ex. 5.5: " + ex55 )

    val ex56a = intStream.headOption
    val ex56b = Empty.headOption
    println( "Ex. 5.6a: " + ex56a )
    println( "Ex. 5.6b: " + ex56b )

    val ex57a = intStream.map( x => x * x )
    val ex57b = intStream.append( ( ) => 6 )
    val ex57c = intStream.filter( _ % 2 == 0 )
    val ex57d = intStream.flatMap { x =>
      if( x % 2 == 0 ) Stream( x, x ) else Stream( x )
    }
    println( "Ex. 5.7a: " + ex57a )
    println( "Ex. 5.7b: " + ex57b )
    println( "Ex. 5.7c: " + ex57c )
    println( "Ex. 5.7d: " + ex57d )

    val ex58 = Stream.constant( Math.PI )
    println( "Ex. 5.8: " + ex58.take( 5 ) )

    val ex59 = Stream.from( 5 )
    println( "Ex. 5.9: " + ex59.take( 5 ) )

    val ex510 = Stream.fibs
    println( "Ex. 5.10: " + ex510.take( 10 ) )

    val ex511 = Stream.unfold( 0 ) { s => Some( (s, s + 1) ) }
    println( "Ex. 5.11: " + ex511.take( 10 ) )

    val ex512a = Stream.constant2( Math.PI )
    val ex512b = Stream.from2( 5 )
    val ex512c = Stream.fibs2
    println( "Ex. 5.12a: " + ex512a.take( 10 ) )
    println( "Ex. 5.12b: " + ex512b.take( 10 ) )
    println( "Ex. 5.12c: " + ex512c.take( 10 ) )

    val ex513a = intStream.map2( x => x * 3 + 12 )
    val ex513b = intStream.take2( 2 )
    val ex513c = intStream.takeWhile3( _ % 2 != 0 )
    val ex513d = intStream.zipWith( doubleStream )( _ * _ )
    val ex513e = intStream.zipAll( doubleStream )
    println( "Ex. 5.13a: " + ex513a.take( 10 ) )
    println( "Ex. 5.13b: " + ex513b.take( 10 ) )
    println( "Ex. 5.13c: " + ex513c.take( 10 ) )
    println( "Ex. 5.13d: " + ex513d.take( 10 ) )
    println( "Ex. 5.13e: " + ex513e.take( 10 ) )

    val ex514 = intStream.startWith( Stream( 1, 2 ) )
    println( "Ex. 5.14: " + ex514 )

    val ex515 = intStream.tails
    println( "Ex. 5.15: " + ex515 )

    val ex516 = intStream.scanRight( 0 )( _ + _ )
    println( "Ex. 5.16: " + ex516 )

    // --- Chapter 6 --- //
    val rng = new SimpleRNG( 42 )

    val ex61 = rng.toStream( SimpleRNG.nonNegativeInt )
    println( "Ex. 6.1: " + ex61.take( 5 ) )
  }

}
