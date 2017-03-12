import scala.io.Source
import scala.collection.breakOut


object phoneMnemonics {

  val in = Source.fromInputStream(getClass.getResourceAsStream("/linuxwords.txt"))
  val words = in.getLines.toList filter( _.forall( _.isLetter ) )

  val mnem = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  val charCode: Map[Char, Char] =
    for ((digit, letters) <- mnem; letter <- letters) yield letter -> digit
  //    ( for ( digit <- mnem.keys; letter <- mnem( digit ) ) yield letter -> digit )( breakOut )
  // ^^ Why does this version not result in a Map unless I use breakOut?

  /** Map a word to its corresponding digit string, e.g. "Java" -> "5282" */
  def wordCode(word: String): String = word.toUpperCase map charCode

  /** A map from digit strings to the words that represent them,
    * "5282" -> List( "Java, "Kata", "Lava", ... )
    * Note: A number with no matches should map to the empty set, e.g. "1111" -> List()
    */
  val wordsForNum: Map[String, Seq[String]] =
    (words groupBy wordCode) withDefaultValue Nil

  /** Return all the ways to encode a number as a list of words */
  def encode( number: String ): Set[List[String]] =
    if ( number.isEmpty ) Set(Nil)
    else
      ( for {
        length <- 1 to number.length
        word <- wordsForNum(number take length)
        rest <- encode(number drop length)
      } yield word :: rest ).toSet

  // ^^ NEVER FORGET! 06/06/2016
  // (The overly-complicated version I made)
  // (Though I *do* remember why I wrote it like that, now: it was because this "correct" version
  // will return lists even when it isn't able to match the end of the number!
  // The version I was working on was using Option so as to distinguish between having reached the
  // end of the number, and having failed to match a word.)

  def translate(phoneNumber: String): Set[String] =
    encode( phoneNumber ) map ( _ mkString " " )

  translate( "7225247386" )

}