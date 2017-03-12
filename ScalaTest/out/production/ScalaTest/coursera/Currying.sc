def sum( f: Int => Int ) : ( Int, Int ) => Int = {
    def sumf( a: Int, b: Int ) : Int = {
        if ( a > b ) 0 else f( a ) + sumf( a + 1, b )
    }
    sumf
}

def product( f: Int => Int )( x: Int, y: Int ) : Int = {
    if ( x > y ) 1 else f( x ) * product( f )( x + 1, y )
}

product( x => x )( 3, 5 )

def factorial( n: Int ) : Int =
product( x => x )( 1, n )

factorial( 5 )

def mapReduce( comb: ( Int, Int ) => Int, default: Int )( f: Int => Int )( x: Int, y: Int ) : Int = {
    if ( x > y ) default else comb( f( x ), mapReduce( comb, default )( f )( x + 1, y ) )
}

mapReduce( ( x, y ) => x * y, 1 )( x => x )( 1, 5 )
