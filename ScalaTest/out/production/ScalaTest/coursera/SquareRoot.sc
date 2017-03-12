//package coursera

object SquareRoot
{
    def sqrt( x: Double ): Double =
    {
        val INITIAL_GUESS = 1
        val ACCEPTABLE_DELTA = 0.0001

        def sqrtIter( guess: Double ) : Double =
            if ( isGoodEnough( guess ) ) guess
            else sqrtIter( refineGuess( guess ) )

        def isGoodEnough( guess: Double ) : Boolean =
            Math.abs( guess * guess - x ) / x < ACCEPTABLE_DELTA

        def refineGuess( guess: Double ) : Double =
            ( guess + x / guess ) / 2

        sqrtIter( INITIAL_GUESS )
    }

    sqrt( 5 )
}
