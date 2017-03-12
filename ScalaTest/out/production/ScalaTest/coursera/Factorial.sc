//package coursera

import scala.annotation.tailrec

object Factorial {
    def factorial( n: Int ) : Int = {
        @tailrec
        def factorialInner( acc: Int, n: Int ) : Int =
            if ( n <= 1 ) acc else factorialInner( acc * n, n - 1 )

        factorialInner( 1, n )
    }

    factorial( 0 )
    factorial( 2 )
    factorial( 7 )
    factorial( 7 )
}