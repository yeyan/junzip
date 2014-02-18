package com.github.junzip

import java.io.File
import org.apache.commons.io.FilenameUtils
import org.apache.commons.io.FileUtils

object ArchiveType extends Enumeration {
  type ArchiveType = Value
  val RAR, SevenZIP, ZIP, TAR = Value
}
import ArchiveType._

class Archive(
    val volumes: Seq[File], val archiveType: ArchiveType, var password: String = null) {

  def keyVolume = volumes.head
  def isMultiVolumeArchive = volumes.size > 1

  def archiveName = {
    val possibleBaseName = FilenameUtils.getBaseName(keyVolume.getAbsolutePath)
    val baseName =
      if (isMultiVolumeArchive)
        FilenameUtils.getBaseName(possibleBaseName)
      else
        possibleBaseName
    val displayType = FilenameUtils.getExtension(keyVolume.getName).toUpperCase
    val displayVolType =
      if (isMultiVolumeArchive)
        "Multi-Vol"
      else
        ""

    s"[$displayVolType $displayType] $baseName"
  }

  def deleteArchive() = {
    for (volume <- volumes) {
      FileUtils.deleteQuietly(volume)
    }
  }
}
