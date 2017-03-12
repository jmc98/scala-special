import sys.process._
import java.net.URL
import java.io.File
import java.nio.file.{Files, Paths}

import scala.collection.convert.WrapAsJava
import scala.io.Source

object TwitchDownloader {
  val BASE_URL = "http://vod.edgecast.hls.ttvnw.net/aee2a9469c/srkevo1_3393230304_3393230304/chunked/"
  val LOCAL_FILES_PATH = "J:/Videos/temp/day/"
  val FILENAME_LENGTH = 27

  def main( args: Array[String] ): Unit = {
    downloadChunks
    createConcatList
//    renameFiles
  }

  def renameFiles = {
    val in = Source.fromFile( LOCAL_FILES_PATH + "index-dvr.m3u8" )
    val filenames = { in.getLines filter( _ startsWith "t" ) map ( _.take( 27 ) ) }.toList
    filenames.indices foreach { num =>
      val srcFilePath = Paths.get( LOCAL_FILES_PATH + filenames( num ) )
      if ( Files.exists( srcFilePath ) )
        Files.move( srcFilePath, Paths.get( LOCAL_FILES_PATH + f"$num%07d" + ".ts" ) ) }
  }

  def createConcatList = {
    val in = Source.fromFile( LOCAL_FILES_PATH + "index-dvr.m3u8" )
    val filenames = in.getLines filter( _ startsWith "t" ) map ( _.take( 27 ) )
    val ffmpegArgs = filenames map ( name => s"file '$name'" )
    Files.write(
      Paths.get( LOCAL_FILES_PATH + "filesInOrder.txt" ),
      WrapAsJava.asJavaIterable( ffmpegArgs.toList ) )
  }

  def downloadChunks = {
    val in = Source.fromFile( LOCAL_FILES_PATH + "index-dvr.m3u8" )
    val filenames = in.getLines filter( _ startsWith "t" ) map ( _.take( 27 ) )
    filenames.filter( name => Files.notExists( Paths.get( LOCAL_FILES_PATH + name ) ) ).foreach {
      name => fileDownloader( BASE_URL + name, LOCAL_FILES_PATH + name )
    }
  }

  def fileDownloader( url: String, filename: String ) = {
    new URL( url ) #> new File( filename ) !!
  }
}