class Rational( n: Int, d: Int ) {
    require( d != 0, "Denominator can't be zero" )

    val div = gcd( n, d )
    val numer = n / div
    val denom = d / div

    def this( n: Int ) = this( n, 1 )

    private def gcd( a: Int, b: Int ): Int =
        if ( b == 0 ) a else gcd( b, a % b )

    def +( r: Rational ) =
        new Rational( numer * r.denom + r.numer * denom,
                      denom * r.denom )

    def -( r: Rational ) = this + -r

    def unary_- = new Rational( -numer, denom )

    def <( r: Rational ) = numer * r.denom < r.numer * denom

    def max( r: Rational ) = if ( this < r ) r else this

    override def toString = numer + "/" + denom
}

val x = new Rational( 1, 3 )
val y = new Rational( 5, 7 )
val z = new Rational( 3, 2 )

// ( 14 - 30 - 63 ) / 42
// -79 / 42

x - y - z
new Rational( 1, 4 ) + new Rational( 2, 8 )
x < y
x max y
y max x
//new Rational( 1, 0 )
