import scala.collection.mutable.ArrayBuffer

object combs {

    def isPrime( n: Int ): Boolean =
        ( 2 until n ) forall  ( d => n % d != 0 )

    // Generate the sequence of all pairs of integers (i, j) such that 1 <= j < i < n,
    // and i + j is prime
    def boundedPrimesSeq( n: Int ): Seq[ ( Int, Int ) ] =
        ( 2 until n ) flatMap ( i =>
            ( 1 until i ) map ( j => ( i, j ) ) ) filter ( x => isPrime( x._1 + x._2 ) )

    def boundedPrimesSeqForV( n: Int ): Seq[ ( Int, Int ) ] =
//        for ( i <- 2 until n; j <- 1 until i if isPrime( i + j ) ) yield ( i, j )
        for { i <- 2 until n
              j <- 1 until i
              if isPrime( i + j )
        } yield ( i, j )


    boundedPrimesSeq( 7 )
    boundedPrimesSeqForV( 7 )

    //    ( ( 1 to 5 ) fold 0 )( ( x: Int, y: Int ) => x + y )
    //    ( ( 1 to 5 ) fold 0 )( _ + _ )

    def scalarProduct( xs: List[Double], ys: List[Double] ): Double =
        ( for { ( x, y ) <- xs zip ys } yield x * y ).sum

    // N-Queens:
    // ---------

    def queens( n: Int ): Set[List[Int]] = {
        def isSafe( col: Int, queens: List[Int] ): Boolean = {
            if ( queens contains col ) false                        // Check columns
            else queens.indices forall(
                row => Math.abs( col - queens( row ) ) != row + 1 ) // Check diagonals
        }

        def placeQueens( k: Int ): Set[List[Int]] =
            if ( k == 1 ) ( ( 0 until n ) map ( List( _ ) ) ).toSet
            else
                for {
                    queens <- placeQueens( k - 1 )
                    col <- 0 until n
                    if isSafe( col, queens )
                } yield col :: queens

        placeQueens( n )
    }

    def printQueens( queens: List[Int] ) =
    {
        queens foreach ( col =>
        {
            val lineChars = Vector.fill( queens.length )( ". " )
            System.out.println( lineChars.updated( col, "@ " ).mkString )
        } )
        System.out.println( "" )
    }

    val q = queens( 8 )
    System.out.println( s"Number of solutions = ${q.size}" )
    q foreach( printQueens( _ ) )

    
}