

def fixedPoint( f: Double => Double )( firstGuess: Double ) = {
    val tolerance = 0.0001
    def isCloseEnough( x: Double, y: Double ) =
        Math.abs( ( x - y ) / x ) < tolerance
    def iterate( guess: Double ): Double = {
        val next = f( guess )
        if ( isCloseEnough( guess, next ) ) next
        else iterate( next )
    }
    iterate( firstGuess )
}

def averageDamp( f: Double => Double )( x: Double ) = ( x + f( x ) ) / 2

// x => ( x + y/x ) / 2

def sqrt( x: Double ) = fixedPoint( averageDamp( y => x / y ) )( 1 )

sqrt( 2 )

//val a : Double = 5;

//def stuff: Int => Int = x => x*x
//stuff == stuff
//stuff eq stuff
//
//val stuff2: Int => Int = x => x*x
//stuff2 == stuff2
//stuff2 eq stuff2


//def stuff: Int => Int = {
//    println("hello")
//    x => x * x
//}
//
//stuff(2)
//val f = stuff
//f(2)
//
//val stuff2: Int => Int = {
//    println("hello")
//    x => x * x
//}
//stuff2(2)

