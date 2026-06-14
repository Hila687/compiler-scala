import java.io.{File, PrintWriter}
import scala.io.StdIn

object JackAnalyzer {

  def main(args: Array[String]): Unit = {
    val folderPath = if (args.nonEmpty) args(0) else {
      println("Enter folder path containing .jack files:")
      StdIn.readLine()
    }

    val folder = new File(folderPath)

    if (!folder.exists() || !folder.isDirectory) {
      println("Error: Invalid folder path.")
      return
    }

    val jackFiles = folder
      .listFiles()
      .filter(file => file.isFile && file.getName.endsWith(".jack"))

    if (jackFiles == null || jackFiles.isEmpty) {
      println("No .jack files found in the given directory.")
      return
    }

    for (jackFile <- jackFiles) {

      // Project 11 output: each .jack file is translated into a .vm file.
      val outputFileName = jackFile.getAbsolutePath.replace(".jack", ".vm")
      val outputFile = new File(outputFileName)
      val writer = new PrintWriter(outputFile)

      try {
        val tokenizer = new JackTokenizer(jackFile)

        // Advance to the first token before starting the compilation process.
        if (tokenizer.hasMoreTokens) {
          tokenizer.advance()
        }

        val vmWriter = new VMWriter(writer)

        // The CompilationEngine performs the actual Jack-to-VM translation.
        val engine = new CompilationEngine(tokenizer, vmWriter)
        engine.compileClass()

        println(s"Successfully created: ${outputFile.getName}")

      } catch {
        case e: Exception =>
          println(s"Error processing file ${jackFile.getName}: ${e.getMessage}")

      } finally {
        writer.close()
      }
    }

    println("Compilation complete!")
  }
}