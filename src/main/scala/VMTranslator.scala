import java.io.File
import java.io.PrintWriter
import scala.io.StdIn

object VMTranslator {

  def main(args: Array[String]): Unit = {

    // Determine the input folder path:
    // If provided as a command-line argument, use it.
    // Otherwise, ask the user to input it manually.
    val folderPath =
      if (args.nonEmpty) args(0)
      else {
        println("Enter folder path:")
        StdIn.readLine()
      }

    val folder = new File(folderPath)

    // Validate that the given path exists and is a directory
    if (!folder.exists() || !folder.isDirectory) {
      println("Error: invalid folder path.")
      return
    }

    // Retrieve all .vm files from the folder
    val vmFiles = getVmFiles(folder)

    // If no VM files were found, stop execution
    if (vmFiles.isEmpty) {
      println("Error: no .vm files found in the folder.")
      return
    }

    // Create the output .asm file in the same folder
    // The output file name is the folder name + ".asm"
    val outputFile = new File(folder, folder.getName + ".asm")

    // Create a writer for writing assembly commands into the output file
    val writer = new PrintWriter(outputFile)

    // Create a single CodeWriter instance for the entire translation process
    val codeWriter = new CodeWriter(writer)

    try {
      // Write bootstrap code at the beginning of the ASM file
      codeWriter.writeInit()

      // Process each VM file in the folder
      for (vmFile <- vmFiles) {
        processFile(vmFile, codeWriter)
      }

      // Print a success message with the output file path
      println(s"Translation completed: ${outputFile.getAbsolutePath}")

    } finally {
      // Ensure that the output file is properly closed
      codeWriter.close()
    }
  }

  // Returns all .vm files inside the given folder, sorted by name
  def getVmFiles(folder: File): Array[File] = {
    val files = folder.listFiles()

    if (files == null) Array.empty
    else
      files
        .filter(file => file.isFile && file.getName.endsWith(".vm"))
        .sortBy(_.getName)
  }

  // Processes a single VM file:
  // - Sets the current file name in CodeWriter (for static segment handling)
  // - Parses each VM command
  // - Delegates translation to CodeWriter
  def processFile(vmFile: File, codeWriter: CodeWriter): Unit = {

    // Extract file name without ".vm" extension
    val fileNameWithoutExtension = removeVmExtension(vmFile.getName)

    // Inform CodeWriter which file is currently being processed
    // This is required for correct translation of the static segment
    codeWriter.setFileName(fileNameWithoutExtension)

    // Create a new Parser for the current VM file
    val parser = new Parser(vmFile)

    // Iterate over all commands in the file
    while (parser.hasMoreCommands) {
      parser.advance()

      // Dispatch translation based on command type
      parser.commandType match {

        case "C_ARITHMETIC" =>
          codeWriter.writeArithmetic(parser.arg1)

        case "C_PUSH" | "C_POP" =>
          codeWriter.writePushPop(
            parser.commandType,
            parser.arg1,
            parser.arg2
          )

        case "C_LABEL" =>
          codeWriter.writeLabel(parser.arg1)

        case "C_GOTO" =>
          codeWriter.writeGoto(parser.arg1)

        case "C_IF" =>
          codeWriter.writeIf(parser.arg1)

        case "C_FUNCTION" =>
          codeWriter.writeFunction(parser.arg1, parser.arg2)

        case "C_RETURN" =>
          codeWriter.writeReturn()

        case "C_CALL" =>
          codeWriter.writeCall(parser.arg1, parser.arg2)

        case _ =>
      }
    }
  }

  // Removes the ".vm" extension from a file name
  def removeVmExtension(fileName: String): String = {
    if (fileName.endsWith(".vm"))
      fileName.substring(0, fileName.length - 3)
    else fileName
  }
}