import java.io.File
import java.io.PrintWriter
import scala.io.StdIn

object VMTranslator {

  def main(args: Array[String]): Unit = {
    val folderPath =
      if (args.nonEmpty) args(0)
      else {
        println("Enter folder path:")
        StdIn.readLine()
      }

    val folder = new File(folderPath)

    if (!folder.exists() || !folder.isDirectory) {
      println("Error: invalid folder path.")
      return
    }

    val vmFiles = getVmFiles(folder)

    if (vmFiles.isEmpty) {
      println("Error: no .vm files found in the folder.")
      return
    }

    val outputFile = new File(folder, folder.getName + ".asm")// Create output file in the same folder with the same name as the folder
    val writer = new PrintWriter(outputFile)

    val codeWriter = new CodeWriter(writer)

    try {
      for (vmFile <- vmFiles) {
        processFile(vmFile, codeWriter)
      }
      println(s"Translation completed: ${outputFile.getAbsolutePath}")
    } finally {
      codeWriter.close()
    }
  }

  def getVmFiles(folder: File): Array[File] = {
    val files = folder.listFiles()
    if (files == null) Array.empty
    else files.filter(file => file.isFile && file.getName.endsWith(".vm")).sortBy(_.getName) // Sort files by name to ensure consistent processing order
  }

  def processFile(vmFile: File, codeWriter: CodeWriter): Unit = {
    val fileNameWithoutExtension = removeVmExtension(vmFile.getName)

    codeWriter.setFileName(fileNameWithoutExtension)

    val parser = new Parser(vmFile)

    while (parser.hasMoreCommands) {
      parser.advance()

      parser.commandType match {
        case "C_ARITHMETIC" =>
          codeWriter.writeArithmetic(parser.arg1)

        case "C_PUSH" | "C_POP" =>
          codeWriter.writePushPop(
            parser.commandType,
            parser.arg1,
            parser.arg2
          )

        case _ =>
        // בתרגיל 1 לא אמורות להיות פקודות אחרות
      }
    }
  }

  def removeVmExtension(fileName: String): String = {
    if (fileName.endsWith(".vm")) fileName.substring(0, fileName.length - 3)
    else fileName
  }
}