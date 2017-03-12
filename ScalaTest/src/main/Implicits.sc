object wrapper {

  class A( val x: Int)

  object B {
    implicit def convertAB( a: A ) = new B( a.x.toString )
  }

  class B( val y: String)

  val a = new A( 5 )

  // Implicit conversion of B => A via the implicit method convertAB of that type
  def printB( b: B ) = println( s"Value is ${b.y}" )

  printB( a )


}