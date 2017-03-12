import scala.collection.mutable

// Something I wrote just for fun!
object TowersOfHanoi {

  import collection.breakOut

  class Hanoi(numDiscs: Int) {
    // Discs 1 to numDiscs
    // Disc 1 is smallest
    // There are 3 rods (0 to 2)
    // Initially, all discs on left-hand rod, and must move them to right-hand rod
    // A larger disc cannot go on top of a smaller disc
    type State = Vector[List[Int]]

    case class Move(from: Int, to: Int)

    type Path = List[Move]

    val initialState: State = Vector((1 to numDiscs).toList, Nil, Nil)
    val solvedState: State = Vector(Nil, Nil, (1 to numDiscs).toList)
    val possibleMoves: List[Move] =
      (for {
        from <- 0 to 2
        to <- 0 to 2
        if from != to
      } yield Move(from, to))(breakOut)

    def isLegal(state: State, move: Move): Boolean = move match {
      case Move(from, to) =>
        state(from).nonEmpty && (state(to).isEmpty || state(to).head > state(from).head)
    }

    def applyMove(state: State, move: Move): State = move match {
      case Move(from, to) =>
        state.updated(from, state(from).tail)
             .updated(to, state(from).head :: state(to))
    }

    var exploredStates: mutable.Map[State, Int] = mutable.Map(initialState -> 0)

    def movesFrom(currentState: State, currentDepth: Int): Stream[Path] = {
      val legalMoves = possibleMoves.filter(move => isLegal(currentState, move))
      val moveStates = legalMoves.zip(legalMoves.map(move => applyMove(currentState, move))) filterNot {
        case (_, newState) => exploredStates.contains(newState) && exploredStates(newState) < currentDepth
      }

      moveStates.flatMap {
        case (move, newState) =>
          exploredStates += newState -> currentDepth
          if (newState == solvedState) Stream(List(move))
          else movesFrom(newState, currentDepth + 1).map(move :: _)
      }(breakOut)
    }

    val solution: List[Move] = movesFrom(initialState, 0).minBy(_.length)
  }

  def main(args: Array[String]): Unit = {
    val sol = new Hanoi(5).solution
    println(s"Solution = ${sol.mkString(",")}")
  }
}