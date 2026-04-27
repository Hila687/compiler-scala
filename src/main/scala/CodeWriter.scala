import java.io.PrintWriter

class CodeWriter(writer: PrintWriter) {

  // Holds the current VM file name (without .vm).
  // Needed for translating the static segment as FileName.index
  private var currentFileName: String = ""

  // Used to generate unique labels for eq / gt / lt commands
  private var labelCounter: Int = 0

  // Updates the current file name before translating a new VM file
  def setFileName(fileName: String): Unit = {
    currentFileName = fileName
  }

  // Translates arithmetic and logical VM commands into Hack assembly
  def writeArithmetic(command: String): Unit = {
    writer.println(s"// $command")

    command match {
      // Binary operations: pop y, use x from stack, store result in x's place
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

      case "xor" =>
        writer.println("@SP")        // טוען את הכתובת של SP
        writer.println("AM=M-1")     // מקטין את SP ב-1, ועכשיו A מצביע ל-y
        writer.println("D=M")        // D = y
        writer.println("A=A-1")      // עובר לאיבר שמתחת, כלומר x
        writer.println("D=D&M")      // D = x & y
        writer.println("@R13")       // ניגש לרגיסטר זמני R13
        writer.println("M=D")        // שומר ב-R13 את x & y

        writer.println("@SP")        // חוזר ל-SP
        writer.println("A=M")        // A מצביע ל-y (כי SP כבר ירד צעד אחד)
        writer.println("D=M")        // D = y
        writer.println("A=A-1")      // עובר ל-x
        writer.println("D=D|M")      // D = x | y

        writer.println("@R13")       // טוען את x & y ששמרנו קודם
        writer.println("D=D-M")      // D = (x | y) - (x & y) = x ^ y

        writer.println("@SP")        // חוזר למחסנית
        writer.println("A=M-1")      // מצביע למקום של x
        writer.println("M=D")        // שומר את תוצאת ה-xor במקום של x

      // Unary operations: apply directly to the top stack value
      case "neg" =>
        writer.println("@SP")
        writer.println("A=M-1")
        writer.println("M=-M")

      case "not" =>
        writer.println("@SP")
        writer.println("A=M-1")
        writer.println("M=!M")

      // Comparison operations: compute x-y and jump according to the result
      case "eq" =>
        writeComparison("JEQ")

      case "gt" =>
        writeComparison("JGT")

      case "lt" =>
        writeComparison("JLT")

      case _ =>
        throw new IllegalArgumentException(s"Unsupported arithmetic command: $command")
    }
  }

  // Translates push/pop VM commands into Hack assembly
  def writePushPop(commandType: String, segment: String, index: Int): Unit = {
    val vmCommand =
      if (commandType == "C_PUSH") s"push $segment $index"
      else s"pop $segment $index"

    writer.println(s"// $vmCommand")

    // push constant i
    if (commandType == "C_PUSH" && segment == "constant") {
      writer.println(s"@$index")
      writer.println("D=A")
      pushDToStack()

      // local / argument / this / that
    } else if (segment == "local" || segment == "argument" || segment == "this" || segment == "that") {
      val base = getBaseSegment(segment)

      if (commandType == "C_PUSH") {
        writer.println(s"@$base")
        writer.println("D=M")
        writer.println(s"@$index")
        writer.println("A=D+A")
        writer.println("D=M")
        pushDToStack()

      } else if (commandType == "C_POP") {
        writer.println(s"@$base")
        writer.println("D=M")
        writer.println(s"@$index")
        writer.println("D=D+A")
        writer.println("@R13")
        writer.println("M=D")

        popStackToD()

        writer.println("@R13")
        writer.println("A=M")
        writer.println("M=D")
      }

      // temp segment: RAM[5]..RAM[12]
    } else if (segment == "temp") {
      val tempAddress = 5 + index

      if (commandType == "C_PUSH") {
        writer.println(s"@$tempAddress")
        writer.println("D=M")
        pushDToStack()

      } else if (commandType == "C_POP") {
        popStackToD()
        writer.println(s"@$tempAddress")
        writer.println("M=D")
      }

      // pointer 0 -> THIS, pointer 1 -> THAT
    } else if (segment == "pointer") {
      val pointerName = getPointerName(index)

      if (commandType == "C_PUSH") {
        writer.println(s"@$pointerName")
        writer.println("D=M")
        pushDToStack()

      } else if (commandType == "C_POP") {
        popStackToD()
        writer.println(s"@$pointerName")
        writer.println("M=D")
      }

      // static i -> FileName.i
    } else if (segment == "static") {
      val staticName = s"$currentFileName.$index"

      if (commandType == "C_PUSH") {
        writer.println(s"@$staticName")
        writer.println("D=M")
        pushDToStack()

      } else if (commandType == "C_POP") {
        popStackToD()
        writer.println(s"@$staticName")
        writer.println("M=D")
      }

    } else {
      throw new IllegalArgumentException(
        s"Unsupported push/pop command: $commandType $segment $index"
      )
    }
  }

  // Translates label commands into Hack assembly labels
  def writeLabel(label: String): Unit = {
    writer.println(s"// label $label")
    writer.println(s"($currentFileName.$label)")
  }

  // Translates goto commands into unconditional jump assembly code
  def writeGoto(label: String): Unit = {
    writer.println(s"// goto $label")
    writer.println(s"@$currentFileName.$label")
    writer.println("0;JMP")
  }

  // Translates if-goto commands into conditional jump assembly code
  def writeIf(label: String): Unit = {
    writer.println(s"// if-goto $label")
    popStackToD()
    writer.println(s"@$currentFileName.$label")
    writer.println("D;JNE")
  }

  // Closes the output writer
  def close(): Unit = {
    writer.close()
  }

  // Returns the Hack base symbol for the given VM segment
  private def getBaseSegment(segment: String): String = {
    segment match {
      case "local"    => "LCL"
      case "argument" => "ARG"
      case "this"     => "THIS"
      case "that"     => "THAT"
      case _ =>
        throw new IllegalArgumentException(s"Invalid base segment: $segment")
    }
  }

  // Maps pointer indices to their corresponding Hack symbols
  private def getPointerName(index: Int): String = {
    index match {
      case 0 => "THIS"
      case 1 => "THAT"
      case _ =>
        throw new IllegalArgumentException(s"Invalid pointer index: $index")
    }
  }

  // Pushes the value currently stored in D onto the stack
  private def pushDToStack(): Unit = {
    writer.println("@SP")
    writer.println("A=M")
    writer.println("M=D")
    writer.println("@SP")
    writer.println("M=M+1")
  }

  // Pops the top stack value into D
  private def popStackToD(): Unit = {
    writer.println("@SP")
    writer.println("AM=M-1")
    writer.println("D=M")
  }

  // Shared implementation for eq / gt / lt
  private def writeComparison(jumpCommand: String): Unit = {
    val trueLabel = s"TRUE_$labelCounter"
    val endLabel = s"END_$labelCounter"
    labelCounter += 1

    popStackToD()              // D = y
    writer.println("A=A-1")    // A points to x
    writer.println("D=M-D")    // D = x - y
    writer.println(s"@$trueLabel")
    writer.println(s"D;$jumpCommand")

    // false case
    writer.println("@SP")
    writer.println("A=M-1")
    writer.println("M=0")
    writer.println(s"@$endLabel")
    writer.println("0;JMP")

    // true case
    writer.println(s"($trueLabel)")
    writer.println("@SP")
    writer.println("A=M-1")
    writer.println("M=-1")

    writer.println(s"($endLabel)")
  }
}