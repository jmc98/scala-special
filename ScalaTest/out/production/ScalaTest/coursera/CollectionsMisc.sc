object misc {

    def combinations( m: Int, n: Int ): Seq[ ( Int, Int ) ] =
    ( 1 to m ) flatMap ( x => ( 1 to n ) map ( y => ( x, y ) )  )


    def isPrime( n: Int ): Boolean =
        ( 2 until n ) forall  ( d => n % d != 0 )

//    isPrime( 9 )
//    isPrime( 8 )
//    isPrime( 7 )
//    isPrime( 6 )
//    isPrime( 5 )
//    isPrime( 4 )
//    isPrime( 3 )
//    isPrime( 2 )
//    isPrime( 1 )

    combinations( 5, 3 )
}