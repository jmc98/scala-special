trait XList[+T] {
    def head: T
    def tail: XList[T]
    def isEmpty: Boolean
    def length: Int
    def ::[U >: T]( elem: U ): XList[U] = new XElem[U]( elem, this )
    def apply( index: Int ): T
}

class XElem[T]( val head: T, val tail: XList[T] ) extends XList[T] {
    override def isEmpty = false
    override def length = 1 + tail.length
    override def toString = head + " " + tail.toString
    override def apply( index: Int ) =
        if ( index == 0 ) head else tail.apply( index - 1 )
}

object XNil extends XList[Nothing] {
    def head = throw new NoSuchElementException( "XNil.head" )
    def tail = throw new NoSuchElementException( "NXil.tail" )
    override def isEmpty: Boolean = true
    override def length = 0
    override def toString = ""
    override def apply( index: Int ) =
        throw new IndexOutOfBoundsException( "Index out of bounds" )
}

object XList {
    def apply[T]() = XNil
    def apply[T]( x1: T ) = x1 :: XNil
    def apply[T]( x1: T, x2: T ) = x1 :: x2 :: XNil
}

val z = 5 :: 3 :: 14 :: 1 :: XNil

z.length
z( 2 )
//z( 4 )



// Other covariance stuff:

class A
class B extends A

class Thing[+T]

var a: A = new A
var b: B = new B
var c: A = b

var ta: Thing[A] = new Thing[A]
var tb: Thing[B] = new Thing[B]
var tc: Thing[A] = tb // Works if Thing[+T] (covariant)
//var td: Thing[B] = ta // Works if Thing[-T] (contravariant)
