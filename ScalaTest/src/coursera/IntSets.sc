

abstract class IntSet
{
    def incl( x: Int ): IntSet

    def union( other: IntSet ): IntSet

    def contains( x: Int ): Boolean
}

object emptySet extends IntSet
{
    override def incl( x: Int ): IntSet = new NonEmptySet( x, emptySet, emptySet )

    override def contains( x: Int ): Boolean = false

    override def toString = ""

    override def union( other: IntSet ): IntSet = other
}

class NonEmptySet( elem: Int, left: IntSet, right: IntSet ) extends IntSet
{
    print( "-" )

    def this( elem: Int ) = this( elem, emptySet, emptySet )

    override def contains( x: Int ): Boolean =
        if ( x < elem ) left contains x
        else if ( x > elem ) right contains x
        else true // must be the value of this node!

    override def incl( x: Int ): IntSet =
        if ( x > elem ) new NonEmptySet( elem, left, right incl x )
        else if ( x < elem ) new NonEmptySet( elem, left incl x, right )
        else this

    override def toString = "{" + left + elem + right + "}"

    override def union( other: IntSet ): IntSet =
    {
        println( "Union of " + this + " and " + other )
        ( ( left union right ) union other ) incl elem
    }
}

//new NonEmptySet( 1 ) incl 2 incl 3
//new NonEmptySet( 2 ) incl 1 incl 3

val t1 = new NonEmptySet( 3 )
val t2 = t1 incl 4

val x = emptySet incl 1 incl 12 incl 6 incl 20
val y = emptySet incl 4 incl 8

x union y
y union x
