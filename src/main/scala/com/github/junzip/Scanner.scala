package com.github.junzip

import java.io.File
import org.apache.commons.io.FilenameUtils

package object scanner {

  import scala.collection.mutable

  trait ArchiveScanner {
    type FunctionList = mutable.ListBuffer[Seq[File] => Seq[Archive]]
    var scanFunctions = new FunctionList()

    def scanArchives(files: Seq[File]): Seq[Archive] = {
      (scanFunctions flatMap (f => f(files))).toList
    }
  }

  trait RARScanner extends ArchiveScanner {

    scanFunctions += (scanRARs)

    def scanRARs(files: Seq[File]): Seq[Archive] = {
      // file scan
      val rarFiles = files filter isRarFile

      // volume scan
      val (singleVolRars, multiVolRars) = rarFiles partition (!isMultiVolumeRarFile(_))

      val singleVolArchives = singleVolRars map (List(_))
      val multiVolArchives = scanMultiVolRars(multiVolRars)

      (singleVolArchives ++ multiVolArchives) map (new Archive(_, ArchiveType.RAR))
    }

    private def isRarFile(file: File): Boolean =
      FilenameUtils.getExtension(file.getName).toLowerCase == "rar"

    private def isMultiVolumeRarFile(file: File): Boolean = {
      val possiblePartName = FilenameUtils.getExtension(
        FilenameUtils.getBaseName(file.getName)).toLowerCase

      (possiblePartName.startsWith("part")
        && possiblePartName.drop(4).forall(Character.isDigit))
    }

    private def scanMultiVolRars(files: Seq[File]): Seq[Seq[File]] = {
      val result = new mutable.ListBuffer[Seq[File]]()
      var tuple: (Seq[File], Seq[File]) = (Seq(), files)

      while (tuple._2.size != 0) {
        val baseName = FilenameUtils.getBaseName(
          FilenameUtils.getBaseName(tuple._2.head.getName))

        tuple = tuple._2 partition (_.getName.startsWith(baseName))
        result += tuple._1
      }

      result
    }

    /*
    type ListBuffer[T] = scala.collection.mutable.ListBuffer[T]
    type ArchiveBuffer = ListBuffer[Archive]

    scanFunctions += scanRARs

    def scanRARs(files: Seq[File]): Seq[Archive] = {
      // file scan
      val rarFiles = files filter isRarFile

      // volume scan
      val (singleVolRars, multiVolRars) = rarFiles partition (!isMultiVolumeRarFile(_))

      val singleVolArchives = singleVolRars map (List(_))
      val multiVolArchives = scanMultiVolRars(multiVolRars)

      (singleVolArchives ++ multiVolArchives) map (new Archive(_, ArchiveType.RAR))
    }


  */
  }
}
