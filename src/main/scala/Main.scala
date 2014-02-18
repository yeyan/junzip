
import java.io._
import org.apache.commons.io.FilenameUtils

import com.github.junzip._
import com.github.junzip.display._
import com.github.junzip.scanner._
import com.github.junzip.unarchiver._

case class Config(
  baseDirectory: File = new File("."),
  targetDirectories: Seq[File] = Seq(),
  deleteArchives: Boolean = false)

trait Options {
  val parser = new scopt.OptionParser[Config]("jzip") {
    head("jzip", "1.0")
    opt[File]('b', "base-directory") valueName ("<directory>") action { (x, c) =>
      c.copy(baseDirectory = x)
    } text ("where we place the unarchive files")
    opt[Unit]('d', "delete-archives") action { (x, c) =>
      c.copy(deleteArchives = true)
    } text ("delete archives after successful extraction")
    help("help") text ("print this usage text")

    arg[File]("<file>...") unbounded () optional () action { (x, c) =>
      c.copy(targetDirectories = c.targetDirectories :+ x)
    } text ("where are the archive files placed") validate { x =>
      if (x.exists && x.isDirectory) success else failure("target directory must exist!")
    }
  }
}

object Main extends App with RARScanner with Unarchiver with Display with Options {

  val terminalType = TerminalType.UnixTerminal
  val banner = List(
    "",
    "junzip 0.1 copyright (C) Ye Yan 2014",
    ""
  )
  var baseDirectory = new File(".")
  var currentProgressLine: ProgressLine = null

  parser.parse(args, new Config()) map { config =>
    baseDirectory = config.baseDirectory

    startDisplay()
    for (targetDirectory <- config.targetDirectories) {
      appendLine(new TextLine(s"Unarchiving all archive files under $targetDirectory"))
      appendLine(new TextLine(""))
      updateDisplay()

      scanArchives(targetDirectory.listFiles.toList) map (archive => {
        currentProgressLine = appendLine(new ProgressLine(archive.archiveName))
        updateDisplay()

        unarchive(archive) match {
          case UnarchiveResult.OK =>
            currentProgressLine.statusCode = ProgressStatus.OK
            if (config.deleteArchives) archive.delete()
          case _ =>
            currentProgressLine.statusCode = ProgressStatus.Failed
        }
        updateDisplay()
      })
    }

    stopDisplay()
  }

  def updateItem(item: ArchiveItem): Unit = {
    //println("extracting = " + item.path)
  }

  def updateProgress(percentage: Double): Unit = {
    currentProgressLine.progress = percentage
    updateDisplay()
  }

}

