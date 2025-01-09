//> using dep com.typesafe.scala-logging::scala-logging:3.9.5
//> using dep ch.qos.logback:logback-classic:1.5.16
//> using dep org.scala-lang.modules::scala-xml:2.3.0

import java.io.File
import scala.xml.XML
import scala.xml.NodeSeq

import com.typesafe.scalalogging.StrictLogging

/**
 * Generate an index of all the IRI stems currently known and other information that might be useful in determining
 * IRI stems for them.
 *
 * SYNOPSIS
 *  scala-cli scripts/GenerateIndex.scala input
 */

object GenerateIndex extends StrictLogging {
  def main(args: Array[String]) = {
    if (args.isEmpty) {
      logger.error("Need at least one argument: the root directory to index from.")
      sys.exit(-1)
    }

    val files = args.flatMap(arg => recursiveListFiles(new File(arg)))
    val xmlFiles = files.filter(_.getName.toLowerCase.endsWith(".xml"))

    // logger.debug(s"Identified ${xmlFiles.length} XML files to index.")

    val results = xmlFiles.flatMap(indexFile)
    val uniqueIdTypes = results.flatMap(_.uniqueIds).map(_._1).sorted.distinct

    println(s"file\trootLabel\tid\turl\tdescription\tpublisher\tstatus\t" + uniqueIdTypes.map(uniqueIdType => f"uniqueIdType=${uniqueIdType}").mkString("\t"))
    results.sortBy(_.id).foreach(res => {
      val uniqueIdMap = res.uniqueIds.toMap
      val byIRIType = uniqueIdTypes.map(typ => uniqueIdMap.getOrElse(typ, "")).mkString("\t")
      println(s"${res.file}\t${res.rootLabel}\t${res.id}\t${res.url}\t${res.descriptionField}\t${res.publisher}\t${res.status}\t${byIRIType}")
    })
  }

  /** Data types */
  case class Result(
    /** The file being indexed. */
    file: File,
    rootLabel: String,
    id: String,
    url: String,
    uniqueIds: Seq[(String, String)],
    description: String,
    publisher: String,
    status: String
  ) {
    /** Escape all the description fields. */
    val descriptionField = description.replaceAll("[\\s\\n\\r]+", " ")
  }

  /** Index files */
  def indexFile(f: File): Option[Result] = {
    // logger.debug(s"Indexing file ${f}")
    try {
      val xml = XML.loadFile(f)
      val id = (xml \ "id" \ "@value").text
      val url = (xml \ "url" \ "@value").text
      val uniqueIds = (xml \ "uniqueId") map {
        case uniqueId => {
          val typ = uniqueId \ "type" \ "@value"
          (typ.text, (uniqueId \ "value" \ "@value").text)
        }
      }
      val description = (xml \ "description" \ "@value").text
      val publisher = (xml \ "publisher" \ "@value").text
      val status = (xml \ "status" \ "@value").text
      Some(Result(f, xml.label, id, url, uniqueIds, description, publisher, status))
    } catch {
      case ex => {
        ex.printStackTrace()
        None
      }
    }
  }

  /** Recurse from files in a given directory.
   * Code from https://stackoverflow.com/a/2638109/27310
   */
  def recursiveListFiles(f: File): Seq[File] = {
    // logger.debug(f"recursiveListFiles(${f})")

    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }
}
