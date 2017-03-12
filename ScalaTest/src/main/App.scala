package main

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source

/**
  * Created by Michael on 30/04/2016.
  */
object App {
  def main( args: Array[String] ) {
    implicit val executionContext = ExecutionContext.global // Just use the global one

    def f1: Future[Unit] = Future { println( "This is F1!" ) }
    def f2: Future[Unit] = Future { println( "This is F2!" ) }
    def f3: Future[Unit] = Future { println( "This is F3!" ) }
    def f4: Future[Unit] = Future { println( "This is F4!" ) }

    // When there are no dependencies:
    Future.sequence( List( f1, f2, f3, f4 ) ) onSuccess { case _ => println( "Finished!" ) }


    // When f4 depends on f3 which depends on f2 which depends on f1
    for {
      fin1 <- f1
      fin2 <- f2
      fin3 <- f3
      fin4 <- f4
    } println( "Finished!" )

    // When f4 depends on f3 and f2, and f3 and f2 both depend on f1
    f1 map { _ => for ( fin2 <- f2; fin3 <- f3 ) f4 } onSuccess { case _ => println( "Finished!" ) }

    Thread.sleep( 1000 )
  }



}
