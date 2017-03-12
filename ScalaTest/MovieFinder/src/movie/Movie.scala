package movie

import scala.xml.Node

case class Movie( title: String, year: String, posterURL: Option[String] )

object Movie {
  def fromXML( movieNode: Node ) =
    Movie(
      title = ( movieNode \ "@title" ).text,
      year = ( movieNode \ "@year" ).text,
      posterURL = ( movieNode \ "@poster" ) collectFirst {    // Store the URL as an Option, since it might not exist
        case node if node.text startsWith "http" => node.text // have seen at least "" and "N/A" for absent poster :)
      } )

  implicit val orderingByYear: Ordering[Movie] = Ordering.by( _.year )
}