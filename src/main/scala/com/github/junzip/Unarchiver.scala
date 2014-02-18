package com.github.junzip

import java.io._
import net.sf.sevenzipjbinding._
import net.sf.sevenzipjbinding.impl._

package object unarchiver {

  object UnarchiveResult extends Enumeration {
    type UnarchiveResult = Value
    val OK, Failed, Unknown = Value
  }
  import UnarchiveResult._

  trait ProgressMonitor {
    def updateItem(item: ArchiveItem): Unit
    def updateProgress(percentage: Double): Unit // 0 < percentage < 1
  }

  trait Unarchiver extends ProgressMonitor {
    var baseDirectory: File
    lazy val unarchive = new UnarchiveFunctor(this, baseDirectory)
  }

  abstract class OpenedArchive extends Closeable {
    val inArchive: ISevenZipInArchive
    val volumes: Array[File]

    lazy val items = {
      (0 until inArchive.getNumberOfItems) map (new ArchiveItem(inArchive, _)) toArray
    }

    def close() = inArchive.close()

    def delete() = {
      import org.apache.commons.io.FileUtils

      close()

      for (vol <- volumes) {
        FileUtils.deleteQuietly(vol)
      }
    }
  }

  implicit def archive2OpenedArchive(archive: Archive): OpenedArchive = {
    if (archive.archiveType == ArchiveType.RAR)
      new OpenedRARArchive(archive)
    else
      throw new UnsupportedOperationException("Not implmented yet!")
  }

  class UnarchiveFunctor(
    val progressMonitor: ProgressMonitor, val baseDirectory: File = new File("."))
      extends IArchiveExtractCallback {

    import org.apache.commons.io.FilenameUtils

    var openedArchive: OpenedArchive = null
    var total: Long = 0
    var operationResult: UnarchiveResult = UnarchiveResult.Unknown

    private def localItem(item: ArchiveItem) =
      new File(FilenameUtils.concat(baseDirectory.getAbsolutePath, item.path))

    private def prepareDirectories() = {
      def createIfMissing(dir: File) = if (!dir.exists) dir.mkdirs()

      createIfMissing(baseDirectory)

      for (item <- openedArchive.items) {
        if (item.isFolder) {
          createIfMissing(localItem(item))
        }
      }
    }

    def setCompleted(completeValue: Long) = {
      progressMonitor.updateProgress(completeValue.toDouble / total)
    }

    def setTotal(total: Long) = {
      this.total = total
    }

    def getStream(index: Int, extractAskMode: ExtractAskMode): ISequentialOutStream = {
      val item = openedArchive.items(index)

      progressMonitor.updateItem(item)

      if (!item.isFolder) {
        class SequentialOutputStream extends ISequentialOutStream with Closeable {
          val fileStream = new FileOutputStream(localItem(item))

          def write(data: Array[Byte]) = {
            fileStream.write(data)
            data.length
          }

          def close() = fileStream.close()
        }

        new SequentialOutputStream()
      } else
        null
    }

    def prepareOperation(extractAskMode: ExtractAskMode) = {}

    def setOperationResult(extractOperationResult: ExtractOperationResult) = {
      extractOperationResult match {
        case ExtractOperationResult.OK => operationResult = UnarchiveResult.OK
        case _ => operationResult = UnarchiveResult.Failed
      }
    }

    def apply(openedArchive: OpenedArchive): UnarchiveResult = {
      try {
        this.openedArchive = openedArchive
        prepareDirectories()
        val in = 0 until openedArchive.inArchive.getNumberOfItems() toArray

        openedArchive.inArchive.extract(in, false, this)
        operationResult
      } finally {
        openedArchive.close()
      }
    }
  }

  trait ArchiveOpenCallback extends IArchiveOpenCallback {
    type JLong = java.lang.Long

    def setCompleted(files: JLong, bytes: JLong): Unit = {}
    def setTotal(files: JLong, bytes: JLong): Unit = {}
  }

  class OpenedRARArchive(archive: Archive)
      extends OpenedArchive with IArchiveOpenVolumeCallback with ArchiveOpenCallback {

    import scala.collection.mutable

    val volumes = archive.volumes.toArray
    val keyVolume = volumes.head
    val cachedFiles = new mutable.HashMap[String, RandomAccessFile]()
    var lastName: String = null

    for (vol <- volumes) {
      cachedFiles.update(vol.getAbsolutePath, new RandomAccessFile(vol, "r"))
    }

    val inArchive = SevenZip.openInArchive(
      null, getStream(keyVolume.getAbsolutePath), this)

    def getProperty(propID: PropID) =
      if (propID == PropID.NAME)
        lastName
      else
        null

    def getStream(filename: String): IInStream = {
      lastName = filename

      cachedFiles.get(filename) match {
        case Some(randomAccessFile) =>
          randomAccessFile.seek(0)
          new RandomAccessFileInStream(randomAccessFile)
        case None =>
          null
      }
    }

    override def close() = {
      super.close()

      for ((name, file) <- cachedFiles) {
        file.close()
      }
    }
  }

  class ArchiveItem(val inArchive: ISevenZipInArchive, val index: Int) {
    private def itemProperty[T](propID: PropID) =
      inArchive.getProperty(index, propID).asInstanceOf[T]

    type Date = java.util.Date

    def path = itemProperty[String](PropID.PATH)
    def isFolder = itemProperty[Boolean](PropID.IS_FOLDER)
    def size = itemProperty[Long](PropID.SIZE)
    def packedSize = itemProperty[Long](PropID.PACKED_SIZE)
    def lastWriteTime = itemProperty[Date](PropID.LAST_WRITE_TIME)
    def creationTime = itemProperty[Date](PropID.CREATION_TIME)
    def lastAccessTime = itemProperty[Date](PropID.LAST_ACCESS_TIME)
    def attributes = itemProperty[Int](PropID.ATTRIBUTES)
    def encrypted = itemProperty[Boolean](PropID.ENCRYPTED)
    def solid = itemProperty[Boolean](PropID.SOLID)
    def commmented = itemProperty[Boolean](PropID.COMMENTED)
    def splitBefore = itemProperty[Boolean](PropID.SPLIT_BEFORE)
    def splitAfter = itemProperty[Boolean](PropID.SPLIT_AFTER)
    def crc = itemProperty[Int](PropID.CRC)
    def hostOS = itemProperty[String](PropID.HOST_OS)
    def method = itemProperty[String](PropID.METHOD)

    override def toString(): String = {
      val formatString: String =
        "[%0" + (Math.log10(inArchive.getNumberOfItems).toInt + 1) + "d]%s"
      formatString.format(index, path)
    }
  }
}
