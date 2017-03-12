import scala.concurrent.{ExecutionContext, Future}

object OnTrackTest {

  implicit val executionContext = ExecutionContext.global // Just use the global one

  def f1: Future[Unit] = Future { println( "This is F1!" ) }
  def f2: Future[Unit] = Future { println( "This is F2!" ) }
  def f3: Future[Unit] = Future { println( "This is F3!" ) }
  def f4: Future[Unit] = Future { println( "This is F4!" ) }


  /**
    * Increment an integer value (encoded as a sequence of digits) by an arbitrary amount
    *
    * @param seq A sequence of single digits representing an integer value,
    *            from most significant to least significant
    * @param inc The amount to increment the sequence value by
    * @return A new sequence containing the updated value
    *
    */
  def incrementNumSeq( seq: Seq[Int], inc: Int ): Seq[Int] = {
    def propagateIncrement( done: Seq[Int], rest: Seq[Int], carried: Int ): Seq[Int] = rest match {
      case Nil => if ( carried == 0 ) done else propagateIncrement( done, Seq( 0 ), carried )
      case ( digit: Int ) :: higherDigits =>
        val newDigit = digit + carried // Might be >= 10
        propagateIncrement( ( newDigit % 10 ) +: done, higherDigits, newDigit / 10 )
    }

    if ( seq == Nil ) Nil                             // If it's empty, just return an empty one
    else propagateIncrement( Nil, seq.reverse, inc )  // Reversing it makes it easier to take the lowest digits
  }


  incrementNumSeq( Nil, 1 )
  incrementNumSeq( Seq( 0 ), 1 )
  incrementNumSeq( Seq( 1, 2, 3 ), 1 )
  incrementNumSeq( Seq( 9, 9, 9 ), 1 )
//  incrementElems( Nil )

  val myStringOption = Some ( "test" )

  myStringOption foreach println                                // Print the value, if it exists
  val newStringOption = myStringOption map ( "This is a " + _ ) // Map it to a new string option
}