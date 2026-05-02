import java.io.File
import scala.io.Source

class Parser(vmFile: File) {

  // פונקציית עזר פרטית שמנקה שורה בודדת
  private def cleanLine(line: String): String = {
    // מחפשים איפה מתחילה הערה
    val commentIndex = line.indexOf("//")

    // אם יש הערה, חותכים את השורה עד אליה. אם לא, לוקחים את כולה.
    val withoutComment = if (commentIndex != -1) line.substring(0, commentIndex) else line

    // מסירים רווחים מיותרים מההתחלה ומהסוף
    withoutComment.trim
  }

  // קריאת הקובץ, ניקוי כל השורות, וסינון השורות שנשארו ריקות
  private val commands: List[String] = {
    val source = Source.fromFile(vmFile) //פותחים את הקובץ לקריאה
    val lines = source.getLines().toList
    source.close() // סוגרים את הקובץ אחרי הקריאה

    lines
      .map(cleanLine)       // מנקים כל שורה
      .filter(_.nonEmpty)   // שומרים רק את השורות שלא נשארו ריקות
  }

  // משתנים פנימיים שיעזרו לנו לעקוב איפה אנחנו נמצאים
  private var currentIndex: Int = -1
  private var currentCommand: String = ""

  // בודקת האם נשארו עוד פקודות לקריאה ברשימה
  def hasMoreCommands: Boolean = {
    // אנחנו בודקות האם האינדקס הבא קטן מאורך הרשימה
    currentIndex + 1 < commands.length
  }

  // מקדמת את הסימנייה לפקודה הבאה ושומרת אותה כפקודה הנוכחית
  def advance(): Unit = {
    if (hasMoreCommands) {
      currentIndex += 1
      currentCommand = commands(currentIndex)
    }
  }

  // נגדיר קבוצה של כל הפקודות האריתמטיות והלוגיות האפשריות
  private val arithmeticCommands = Set("add", "sub", "neg", "eq", "gt", "lt", "and", "or", "not","xor")

  // הפונקציה שמחזירה את סוג הפקודה
  def commandType: String = {
    val firstWord = currentCommand.split("\\s+").head

    firstWord match {
      case cmd if arithmeticCommands.contains(cmd) => "C_ARITHMETIC"
      case "push"     => "C_PUSH"
      case "pop"      => "C_POP"
      case "label"    => "C_LABEL"
      case "goto"     => "C_GOTO"
      case "if-goto"  => "C_IF"
      case "function" => "C_FUNCTION"
      case "call"     => "C_CALL"
      case "return"   => "C_RETURN"
      case _          => "UNKNOWN_COMMAND"
    }
  }

  // מחזירה את הארגומנט הראשון של הפקודה
  def arg1: String = {
    val parts = currentCommand.split("\\s+")
    val typeOfCommand = commandType

    typeOfCommand match {
      case "C_ARITHMETIC" =>
        parts(0)

      case "C_PUSH" | "C_POP" | "C_LABEL" | "C_GOTO" | "C_IF" | "C_FUNCTION" | "C_CALL" =>
        parts(1)

      case "C_RETURN" =>
        throw new IllegalStateException("arg1 should not be called for C_RETURN")

      case _ =>
        throw new IllegalArgumentException(s"Unknown command type: $typeOfCommand")
    }
  }

  // מחזירה את הארגומנט השני של הפקודה (רלוונטי רק ל-push ו-pop)
  def arg2: Int = {
    val parts = currentCommand.split("\\s+")
    val typeOfCommand = commandType

    typeOfCommand match {
      case "C_PUSH" | "C_POP" | "C_FUNCTION" | "C_CALL" =>
        parts(2).toInt

      case _ =>
        throw new IllegalStateException(s"arg2 should not be called for $typeOfCommand")
    }
  }
}