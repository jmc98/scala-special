object idealised
{
    abstract class Bool
    {
        protected def ifThenElse( ifVal: Bool, elseVal: Bool ): Bool
        def &&( x: Bool ): Bool = ifThenElse( x, False )
        def ||( x: Bool ): Bool = ifThenElse( True, x )
        def unary_! : Bool = ifThenElse( False, True )
        def ==( x: Bool ): Bool = ifThenElse( x, x.unary_! )
        def !=( x: Bool ): Bool = ifThenElse( x.unary_!, x )
        def <( x: Bool ): Bool = ifThenElse( False, x )
        def >( x: Bool ): Bool = ifThenElse( !x, False )
    }

    object True extends Bool
    {
        override protected def ifThenElse( ifVal: Bool, elseVal: Bool ): Bool = ifVal
        override def toString = "True"
    }

    object False extends Bool
    {
        override protected def ifThenElse( ifVal: Bool, elseVal: Bool ): Bool = elseVal
        override def toString = "False"
    }

    abstract class Nat {
        def isZero: Boolean
        def predecessor: Nat
        def successor: Nat = new Succ( this )
        def +( x: Nat ): Nat
        def -( x: Nat ): Nat
    }

    object Zero extends Nat {
        def isZero = true
        def predecessor = throw new NoSuchElementException( "Negative number" )
        override def toString = ""
        def +( x: Nat ): Nat = x
        def -( x: Nat ): Nat = if ( x isZero ) this else predecessor
    }

    class Succ( n: Nat ) extends Nat {
        def isZero: Boolean = false
        def predecessor: Nat = n
        def +( x: Nat ): Nat = predecessor + x.successor
        def -( x: Nat ): Nat = if ( x isZero ) this else predecessor - x.predecessor
        override def toString = "#" + predecessor.toString
    }

    val two = Zero.successor.successor
    val three = Zero.successor.successor.successor

    two + three
    three - two

    True == True
    False == False
    False == True

    True || False
    False || False
    True && !False

}