package movie

object Application {
  def main( args: Array[String] ): Unit = {
    if ( args.length != 1 ) {
      println( "Expected 1 argument, but found " + args.length )
      System.exit( -1 )
    }
    val movies = new MovieFinder().findMoviesWithPosters( args( 0 ) ).sorted
    movies foreach( m => println( s"${m.title} [${m.year}] - ${m.posterURL.get}" ) )
    println( s"=> ${movies.size} result(s) found" )
  }
}
