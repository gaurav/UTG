//> using dep com.typesafe.scala-logging::scala-logging:3.9.5
//> using dep ch.qos.logback:logback-classic:1.4.7

import java.io.File
import com.typesafe.scalalogging.StrictLogging

object GenerateIndex extends StrictLogging {
  def main(args: Array[String]) = {
    if (args.length != 1) {
      logger.error("Need one argument: the root directory to index from.")
      sys.exit(-1)
    }

    val files = recursiveListFiles(new File(args(0)))
    val xmlFiles = files.filter(_.getName.toLowerCase.endsWith(".xml"))

    logger.info(s"Identified ${xmlFiles.length} XML files to index.")

    xmlFiles.foreach(indexFile)
  }

  /** Data types */
  case class Result(
    /** The file being indexed. */
    file: File
  )

  /** Index files */
  def indexFile(f: File): Option[Result] = {
    logger.debug(s"Indexing file ${f}")
    None
  }

  /** Recurse from files in a given directory.
   * Code from https://stackoverflow.com/a/2638109/27310
   */ 
  def recursiveListFiles(f: File): Seq[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }
}
