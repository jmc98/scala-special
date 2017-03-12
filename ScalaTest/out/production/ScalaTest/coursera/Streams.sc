def streamRange( low: Int, high: Int ): Stream[Int] =
  if ( low >= high ) Stream.empty
  else low #:: streamRange( low + 1, high )

streamRange( 2, 5 )

def ints( from: Int ): Stream[Int] = from #:: ints( from + 1 )

def nats = ints( 0 )
def mults = nats map ( _ * 4 )

def sieve( in: Stream[Int] ): Stream[Int] =
in.head #:: sieve( in.tail filter( _ % in.head != 0 ) )

sieve( ints( 2 ) ) take 10 toList

def sqrtStream( x: Double ): Stream[Double] = {
  def improve( guess: Double ) = ( guess + x / guess ) / 2
  def guesses: Stream[Double] = 1 #:: ( guesses map improve )
  guesses
} // His version

def sqrtStream2( x: Double ): Stream[Double] = {
  def improve( guess: Double ) = ( guess + x / guess ) / 2
  def guesses( start: Double ): Stream[Double] = start #:: guesses( improve( start ) )
  guesses(1)
} // My version -- much faster!
// His version slows down more the longer the sequence is, because it starts with "1" at
// EVERY STEP in the sequence and then "map improve"s it more and more times each time.
// Instead, my version simply improves the previous guess in the sequence ONCE

// By 5000 guesses, the time difference is already > 100x
val t1 = System.nanoTime()
sqrtStream( 5 ) take 5000 toList
val t2 = System.nanoTime()
sqrtStream2( 5 ) take 5000 toList
val t3 = System.nanoTime()

t2-t1
t3-t2