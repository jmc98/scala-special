def sum( f: Int => Int, x: Int, y: Int ) : Int = {
    def accumulate( total: Int, next: Int ) : Int =
        if ( next > y ) total else accumulate( total + f( next ), next + 1 )

    accumulate( 0, x )
}

sum( a => a * a, 3, 5 )