object matching
{

    trait Expr
    {
        def eval: Int = this match
        {
            case Num( x ) => x
            case Sum( x, y ) => x.eval + y.eval
            case Prod( x, y ) => x.eval * y.eval
        }

        override def toString: String = {
            def maybeWrap( x: Expr ): String = x match {
                case Sum( _, _ ) => "(" + x.toString + ")"
                case _ => x.toString // Or default case, if he'd taught that!
            }

            this match {
                case Num( x ) => x.toString
                case Sum( x, y ) => x.toString + " + " + y.toString
                case Prod( x, y ) => maybeWrap( x ) + " * " + maybeWrap( y )
                case Var( x ) => x
            }
        }
    }

    case class Num( x: Int ) extends Expr

    case class Sum( x: Expr, y: Expr ) extends Expr

    case class Prod( x: Expr, y: Expr ) extends Expr

    case class Var( x: String ) extends Expr

    val a = Sum( Num( 5 ), Sum( Num( 4 ), Num( 1 ) ) )
    a.eval

    val b = Sum( Prod( Num( 2 ), Var( "x" ) ), Var( "y" ) )
    val c = Prod( Sum( Num( 2 ), Var( "x" ) ), Var( "y" ) )

}