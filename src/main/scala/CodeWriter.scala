import java.io.PrintWriter

class CodeWriter(writer: PrintWriter) {

  private var currentFileName: String = ""
  private var labelCounter: Int = 0

  def setFileName(fileName: String): Unit = {
    currentFileName = fileName
  }

  def writeArithmetic(command: String): Unit = {
    writer.println(s"// $command")

    command match {
      case "add" =>
        writer.println("@SP")
        writer.println("AM=M-1")
        writer.println("D=M")
        writer.println("A=A-1")
        writer.println("M=M+D")

      case "sub" =>
        writer.println("@SP")
        writer.println("AM=M-1")
        writer.println("D=M")
        writer.println("A=A-1")
        writer.println("M=M-D")

      case "neg" =>
        writer.println("@SP")
        writer.println("A=M-1")
        writer.println("M=-M")

      case "and" =>
        writer.println("@SP")
        writer.println("AM=M-1")
        writer.println("D=M")
        writer.println("A=A-1")
        writer.println("M=M&D")

      case "or" =>
        writer.println("@SP")
        writer.println("AM=M-1")
        writer.println("D=M")
        writer.println("A=A-1")
        writer.println("M=M|D")

      case "not" =>
        writer.println("@SP")
        writer.println("A=M-1")
        writer.println("M=!M")

      case "eq" =>
        val trueLabel = s"TRUE_$labelCounter"
        val endLabel = s"END_$labelCounter"
        labelCounter += 1

        writer.println("@SP")
        writer.println("AM=M-1")
        writer.println("D=M")
        writer.println("A=A-1")
        writer.println("D=M-D")
        writer.println(s"@$trueLabel")
        writer.println("D;JEQ")
        writer.println("@SP")
        writer.println("A=M-1")
        writer.println("M=0")
        writer.println(s"@$endLabel")
        writer.println("0;JMP")
        writer.println(s"($trueLabel)")
        writer.println("@SP")
        writer.println("A=M-1")
        writer.println("M=-1")
        writer.println(s"($endLabel)")

      case "gt" =>
        val trueLabel = s"TRUE_$labelCounter"
        val endLabel = s"END_$labelCounter"
        labelCounter += 1

        writer.println("@SP")
        writer.println("AM=M-1")
        writer.println("D=M")
        writer.println("A=A-1")
        writer.println("D=M-D")
        writer.println(s"@$trueLabel")
        writer.println("D;JGT")
        writer.println("@SP")
        writer.println("A=M-1")
        writer.println("M=0")
        writer.println(s"@$endLabel")
        writer.println("0;JMP")
        writer.println(s"($trueLabel)")
        writer.println("@SP")
        writer.println("A=M-1")
        writer.println("M=-1")
        writer.println(s"($endLabel)")

      case "lt" =>
        val trueLabel = s"TRUE_$labelCounter"
        val endLabel = s"END_$labelCounter"
        labelCounter += 1

        writer.println("@SP")
        writer.println("AM=M-1")
        writer.println("D=M")
        writer.println("A=A-1")
        writer.println("D=M-D")
        writer.println(s"@$trueLabel")
        writer.println("D;JLT")
        writer.println("@SP")
        writer.println("A=M-1")
        writer.println("M=0")
        writer.println(s"@$endLabel")
        writer.println("0;JMP")
        writer.println(s"($trueLabel)")
        writer.println("@SP")
        writer.println("A=M-1")
        writer.println("M=-1")
        writer.println(s"($endLabel)")

      case _ =>
        throw new IllegalArgumentException(s"Unsupported arithmetic command: $command")
    }
  }

  def writePushPop(commandType: String, segment: String, index: Int): Unit = {
    writer.println(s"// $commandType $segment $index")

    if (commandType == "C_PUSH" && segment == "constant") {
      writer.println(s"@$index")
      writer.println("D=A")
      writer.println("@SP")
      writer.println("A=M")
      writer.println("M=D")
      writer.println("@SP")
      writer.println("M=M+1")

    } else if (segment == "local" || segment == "argument" || segment == "this" || segment == "that") {
      val base = getBaseSegment(segment)

      if (commandType == "C_PUSH") {
        writer.println(s"@$base")
        writer.println("D=M")
        writer.println(s"@$index")
        writer.println("A=D+A")
        writer.println("D=M")
        writer.println("@SP")
        writer.println("A=M")
        writer.println("M=D")
        writer.println("@SP")
        writer.println("M=M+1")

      } else if (commandType == "C_POP") {
        writer.println(s"@$base")
        writer.println("D=M")
        writer.println(s"@$index")
        writer.println("D=D+A")
        writer.println("@R13")
        writer.println("M=D")

        writer.println("@SP")
        writer.println("AM=M-1")
        writer.println("D=M")

        writer.println("@R13")
        writer.println("A=M")
        writer.println("M=D")
      }

    } else if (segment == "temp") {
    val tempAddress = 5 + index

    if (commandType == "C_PUSH") {
      writer.println(s"@$tempAddress")
      writer.println("D=M")
      writer.println("@SP")
      writer.println("A=M")
      writer.println("M=D")
      writer.println("@SP")
      writer.println("M=M+1")

    } else if (commandType == "C_POP") {
      writer.println("@SP")
      writer.println("AM=M-1")
      writer.println("D=M")
      writer.println(s"@$tempAddress")
      writer.println("M=D")
    }

  } else if (segment == "pointer") {
    val pointerName = getPointerName(index)

    if (commandType == "C_PUSH") {
      writer.println(s"@$pointerName")
      writer.println("D=M")
      writer.println("@SP")
      writer.println("A=M")
      writer.println("M=D")
      writer.println("@SP")
      writer.println("M=M+1")

    } else if (commandType == "C_POP") {
      writer.println("@SP")
      writer.println("AM=M-1")
      writer.println("D=M")
      writer.println(s"@$pointerName")
      writer.println("M=D")
    }

  } else if (segment == "static") {
      val staticName = s"$currentFileName.$index"

      if (commandType == "C_PUSH") {
        writer.println(s"@$staticName")
        writer.println("D=M")
        writer.println("@SP")
        writer.println("A=M")
        writer.println("M=D")
        writer.println("@SP")
        writer.println("M=M+1")

      } else if (commandType == "C_POP") {
        writer.println("@SP")
        writer.println("AM=M-1")
        writer.println("D=M")
        writer.println(s"@$staticName")
        writer.println("M=D")
      }
    } else {
      throw new IllegalArgumentException(
        s"Unsupported push/pop command: $commandType $segment $index"
      )
    }
  }

  def close(): Unit = {
    writer.close()
  }

  private def getBaseSegment(segment: String): String = {
    segment match {
      case "local"    => "LCL"
      case "argument" => "ARG"
      case "this"     => "THIS"
      case "that"     => "THAT"
      case _ => throw new IllegalArgumentException(s"Invalid base segment: $segment")
    }
  }

  private def getPointerName(index: Int): String = {
    index match {
      case 0 => "THIS"
      case 1 => "THAT"
      case _ => throw new IllegalArgumentException(s"Invalid pointer index: $index")
    }
  }
}