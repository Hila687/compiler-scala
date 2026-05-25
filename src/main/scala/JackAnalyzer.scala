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

    val jackFiles = folder.listFiles().filter(file => file.isFile && file.getName.endsWith(".jack"))

    if (jackFiles == null || jackFiles.isEmpty) {
      println("No .jack files found in the given directory.")
      return
    }

    for (jackFile <- jackFiles) {

      // שינוי 1: הפלט הוא עכשיו xxx.xml (בלי ה-T), בדיוק לפי ההוראות
      val outputFileName = jackFile.getAbsolutePath.replace(".jack", ".xml")
      val outputFile = new File(outputFileName)
      val writer = new PrintWriter(outputFile)

      try {
        val tokenizer = new JackTokenizer(jackFile)

        // שינוי 2: אנחנו חייבות לקרוא לטוקן הראשון כדי שהמנוע יוכל להתחיל לעבוד
        if (tokenizer.hasMoreTokens) {
          tokenizer.advance()
        }

        // שינוי 3: יצירת ה-CompilationEngine והפעלת החוק הראשי (class)
        val engine = new CompilationEngine(tokenizer, writer)
        engine.compileClass()

        println(s"Successfully created: ${outputFile.getName}")

      } catch {
        case e: Exception => println(s"Error processing file ${jackFile.getName}: ${e.getMessage}")
      } finally {
        writer.close()
      }
    }

    println("Parsing complete!")
  }
}