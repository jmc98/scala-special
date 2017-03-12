def squareList( xs: List[Int] ): List[Int] = xs match {
    case Nil => xs
    case y :: ys => ( y * y ) :: squareList( ys )
}

def squareList2( xs: List[Int] ): List[Int] = xs map ( x => x * x )


def pack[T]( xs: List[T] ): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 => {
        val ( same, rest ) = xs span ( y => y == x )
        same :: pack( rest )
    }
}

def encode[T]( xs: List[T] ): List[( T, Int )] =
    pack( xs ) map ( ps => ( ps.head, ps.length ) )

def reduceLeft[T]( xs: List[T], op: ( T, T ) => T ): T = xs match {
    case Nil => throw new Error( "Can't reduce empty list" )
    case List( x ) => x
    case default => op( xs.last, reduceLeft( xs.init, op ) )
}


val x = List( 1, 1, 3, 5, 5, 5, 3, 9, 0, 0 )
encode( x )

reduceLeft( x, ( a: Int, b: Int ) => a + b )
