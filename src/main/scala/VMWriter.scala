import java.io.PrintWriter

// Responsible only for writing VM commands to the output file.
class VMWriter(writer: PrintWriter) {

  // Writes a VM push command.
  def writePush(segment: String, index: Int): Unit = {
    writer.println(s"push $segment $index")
  }

  // Writes a VM pop command.
  def writePop(segment: String, index: Int): Unit = {
    writer.println(s"pop $segment $index")
  }

  // Writes a VM arithmetic/logical command.
  def writeArithmetic(command: String): Unit = {
    writer.println(command)
  }

  // Writes a VM label command.
  def writeLabel(label: String): Unit = {
    writer.println(s"label $label")
  }

  // Writes a VM goto command.
  def writeGoto(label: String): Unit = {
    writer.println(s"goto $label")
  }

  // Writes a VM if-goto command.
  def writeIf(label: String): Unit = {
    writer.println(s"if-goto $label")
  }

  // Writes a VM call command.
  def writeCall(name: String, nArgs: Int): Unit = {
    writer.println(s"call $name $nArgs")
  }

  // Writes a VM function declaration.
  def writeFunction(name: String, nLocals: Int): Unit = {
    writer.println(s"function $name $nLocals")
  }

  // Writes a VM return command.
  def writeReturn(): Unit = {
    writer.println("return")
  }

  // Closes the underlying writer.
  def close(): Unit = {
    writer.close()
  }
}