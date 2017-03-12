import scala.collection.breakOut

object polynomials {

    class Poly( val terms0: Map[Int, Double] ) {
        val terms = terms0 withDefaultValue 0.0

        def this( bindings: ( Int, Double )* ) = this( bindings.toMap )

        override def toString =
            ( for ( ( exp, coef ) <- terms.toList.sorted.reverse )
                yield s"${coef}x^$exp" ) mkString " + "

        def + ( other: Poly ) =
            new Poly(
                ( for ( k <- terms.keySet ++ other.terms.keySet )
                    yield k -> ( terms( k ) + other.terms( k ) ) )
                    ( breakOut[ Set[Int], ( Int, Double ), Map[ Int, Double ] ] ) )

        // Alt version of +
        def plus ( other: Poly ) =
            new Poly( ( other.terms foldLeft terms )( addTerm ) )

        def addTerm( terms: Map[Int, Double], term: ( Int, Double ) ) =
            terms updated( term._1, term._2 + terms( term._1 ) )

    }

    val p1 = new Poly( 1 -> 2.0, 3 -> 4.0, 5 -> 6.2 )
    val p2 = new Poly( 0 -> 3.0, 3 -> 7.0 )
    p1 + p2
    p1 plus p2

}