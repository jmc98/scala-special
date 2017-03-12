package movie

import scala.annotation.tailrec
import scala.xml.XML

class MovieFinder {

  def findMoviesWithPosters( titleTerm: String ): Seq[Movie] = {
    @tailrec
    def getAllMovies( titleTerm: String, currentPage: Int = 1,
                      moviesFromPrevPages: Seq[Movie] = Seq(), numMovies: Option[Int] = None ): Seq[Movie] = {
      println("Retrieving page " + currentPage)
      val queryResult = XML.load( s"http://www.omdbapi.com/?s=$titleTerm&type=movie&page=$currentPage&r=xml" )
      val moviesIncThisPage = moviesFromPrevPages ++ ( queryResult.child map Movie.fromXML )
      val totalMovies = if ( numMovies.isEmpty ) ( queryResult \ "@totalResults" ).text.toInt else numMovies.get
      if ( moviesIncThisPage.size >= totalMovies ) moviesIncThisPage
      else getAllMovies( titleTerm, currentPage + 1, moviesIncThisPage, numMovies )
    }

    getAllMovies( titleTerm ) filter ( _.posterURL isDefined )
  }

}
