package com.github.junzip

package object display {
  import com.googlecode.lanterna._
  import com.googlecode.lanterna.screen._
  import com.googlecode.lanterna.terminal._

  trait DisplayLine {
    def drawLine(display: Display, row: Int): Unit
  }

  object TerminalType extends Enumeration {
    type TerminalType = Value
    val SwingTerminal, UnixTerminal = Value
  }
  import TerminalType._

  trait Display {
    val banner: List[String]
    val terminalType: TerminalType

    lazy val screen = {
      val terminal =
        if (terminalType == UnixTerminal)
          TerminalFacade.createTextTerminal()
        else
          TerminalFacade.createTerminal()

      TerminalFacade.createScreen(terminal)
    }

    lazy val writer = new ScreenWriter(screen)
    var screenBuffer = new scala.collection.mutable.Queue[DisplayLine]

    def screenWidth = screen.getTerminalSize.getColumns
    def screenHight = screen.getTerminalSize.getRows

    def startDisplay() = screen.startScreen()
    def stopDisplay() = screen.stopScreen()

    def updateDisplay() = {
      screen.clear()

      var currentRow = screenHight - screenBuffer.size - banner.size

      for (line <- banner) {
        writer.drawString(0, currentRow, line)
        currentRow += 1
      }

      for (item <- screenBuffer) {
        item.drawLine(this, currentRow)
        currentRow += 1
      }

      screen.refresh()
    }

    def appendLine[T <: DisplayLine](newLine: T): T = {
      screenBuffer enqueue newLine

      if (screenBuffer.size > screenHight - banner.size)
        screenBuffer dequeue

      newLine
    }
  }

  class TextLine(var text: String) extends DisplayLine {
    override def drawLine(display: Display, row: Int) = {
      display.writer.drawString(0, row, text)
    }
  }

  object ProgressStatus extends Enumeration {
    type ProgressStatus = Value
    val OK, Failed, InProgress = Value
  }
  import ProgressStatus._

  class ProgressLine(
      text: String,
      var statusCode: ProgressStatus = ProgressStatus.InProgress,
      var progress: Double = 0) extends TextLine(text) {

    def statusString =
      statusCode match {
        case OK | Failed =>
          statusCode.toString
        case InProgress =>
          f"${progress * 100}%2.1f" + "%"
      }

    override def drawLine(display: Display, row: Int) = {
      super.drawLine(display, row)

      val status = statusString
      val col = display.screenWidth - status.length - 2

      display.writer.drawString(col, row, "[")

      val color = statusCode match {
        case Failed =>
          Terminal.Color.RED
        case InProgress =>
          Terminal.Color.YELLOW
        case _ =>
          Terminal.Color.DEFAULT
      }
      display.writer.setForegroundColor(color)
      display.writer.drawString(col + 1, row, status)
      display.writer.setForegroundColor(Terminal.Color.DEFAULT)

      display.writer.drawString(display.screenWidth - 1, row, "]")
    }
  }
}
