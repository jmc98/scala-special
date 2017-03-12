object WaterPouringProblem {

  class Pouring( capacity: Vector[Int] ) {
    // States
    type State = Vector[Int]
    val initialState = capacity map ( _ => 0 )

    //Vector.fill( capacity.length )( 0 )

    //Moves

    trait Move {
      def change( state: State ): State
    }

    case class Empty( glass: Int ) extends Move {
      override def change( state: State ) = state updated(glass, 0)
    }

    case class Fill( glass: Int ) extends Move {
      override def change( state: State ) = state updated(glass, capacity( glass ))
    }

    case class Pour( from: Int, to: Int ) extends Move {
      override def change( state: State ): State = {
        val amountPoured = Math.min( state( from ), capacity( to ) - state( to ) )
        ( state updated(from, state( from ) - amountPoured) )
          .updated( to, state( to ) + amountPoured )
      }
    }

    val glasses = capacity.indices
    // better than "0 until capacity.length"
    val moves =
      ( for ( g <- glasses ) yield Empty( g ) ) ++
        ( for ( g <- glasses ) yield Fill( g ) ) ++
        ( for ( from <- glasses; to <- glasses; if from != to ) yield Pour( from, to ) )

    // Paths
    class Path( history: List[Move], val endState: State ) {
//      val endState: State = ( history foldRight initialState ) ( ( move, state ) => move change state )

      // Older recursive version with auxiliary function trackState:
      // def endState: State = trackState( history )
      //      private def trackState( history: List[Move] ): State = history match {
      //        case Nil => initialState
      //        case nextMove :: remainingMoves => nextMove change trackState( remainingMoves )
      //      }
      def extend( move: Move ) = new Path( move :: history, move change endState )

      override def toString = ( history.reverse mkString " " ) + "--> " + endState
    }

    def from( paths: Set[Path], explored: Set[State] ): Stream[Set[Path]] =
      if ( paths.isEmpty ) Stream.empty
      else {
        val derivedPaths = for {
          path <- paths
          nextPath <- moves map path.extend
          if !explored.contains( nextPath.endState )
        } yield nextPath
        paths #:: from( derivedPaths, explored ++ derivedPaths.map( _.endState ) )
      }

    val initialPath = new Path( Nil, initialState )

    val pathSets = from( Set( initialPath ), Set( initialState ) )

    def solution( targetVolume: Int ): Stream[Path] =
      pathSets.flatten filter ( _.endState contains targetVolume )
  }



  val problem = new Pouring( Vector( 4, 9 ) )
  problem.moves
  ( problem.pathSets take 3 ).toList

  problem.solution( 6 )
}