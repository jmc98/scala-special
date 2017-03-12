
def isort( xs: List[Int] ): List[Int] = xs match {
    case List() => xs
    case x :: xt => insert( x, isort( xt ) )
}

def msort[T]( xs: List[T] )( implicit ord: Ordering[T] ): List[T] = {
    val n = xs.length / 2
    if ( n == 0 ) xs
    else {
        def merge( xs: List[T], ys: List[T] ): List[T] = ( xs, ys ) match {
            case ( Nil, ys ) => ys
            case ( xs, Nil ) => xs
            case ( x :: xs1, y :: ys1 ) =>
                if ( ord.lt( x, y ) ) x :: merge( xs1, ys ) else y :: merge( xs, ys1 )
        }
        val ( first, second ) = xs splitAt n
        merge( msort( first )( ord ), msort( second )( ord ) )
    }
}

def insert( x: Int, xs: List[Int] ): List[Int] = xs match {
    case List() => List( x )
    case y :: ys => if ( x <= y ) x :: xs else y :: insert( x, ys )
}

val a = List( 1, 15, 9, 12, 11, 2, 3, 20 )
isort( a )
msort( a )

def last[T]( xs: List[T] ): T = xs match {
    case List() => throw new Error( "last of empty list" ) // Should it be Error?
    case List( x ) => x
    case y :: ys => last( ys )
}

def init[T]( xs: List[T] ): List[T] = xs match {
    case List() => throw new Error( "init of empty list" ) // Should it be Error?
    case List( x ) => Nil // Why does he always use List() in the course?
    case y :: ys => y :: init( ys )
}

def concat[T]( xs: List[T], ys: List[T] ): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat( zs, ys )
}

def reverse[T]( xs: List[T] ): List[T] = xs match {
    case List() => Nil
    case y :: ys => reverse( ys ) ++ List( y )
}

def removeAt[T]( xs: List[T], n: Int ) = ( xs take n ) ::: ( xs drop n + 1 )

//def removeAt[T]( xs: List[T], n: Int ): List[T] = xs match {
//    case List() => Nil
//    case y :: ys => if ( n == 0 ) ys else y :: remoteAt( ys, n - 1 )
//}

val r = List( "aye", "bee", "see" )
var s = List( "dee", "eee", "eff" )

concat( r, s )
//r ::: s

removeAt( r, 2 )
