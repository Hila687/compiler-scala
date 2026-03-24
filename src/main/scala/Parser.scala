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
  private val arithmeticCommands = Set("add", "sub", "neg", "eq", "gt", "lt", "and", "or", "not")

  // הפונקציה שמחזירה את סוג הפקודה
  def commandType: String = {
    // נחלק את הפקודה הנוכחית למילים לפי רווחים, וניקח את המילה הראשונה
    val firstWord = currentCommand.split(" ").head

    // נבדוק לאיזו משפחה המילה הראשונה שייכת
    if (arithmeticCommands.contains(firstWord)) {
      "C_ARITHMETIC"
    } else if (firstWord == "push") {
      "C_PUSH"
    } else if (firstWord == "pop") {
      "C_POP"
    } else {
      "UNKNOWN_COMMAND" // למקרה של פקודה לא מוכרת
    }
  }

  // מחזירה את הארגומנט הראשון של הפקודה
  def arg1: String = {
    val typeOfCommand = commandType

    if (typeOfCommand == "C_ARITHMETIC") {
      // בפקודות אריתמטיות, הארגומנט הראשון הוא הפקודה עצמה (המילה הראשונה)
      currentCommand.split(" ").head
    } else {
      // בפקודות push או pop, הארגומנט הראשון הוא הסגמנט (המילה השנייה)
      currentCommand.split(" ")(1)
    }
  }

  // מחזירה את הארגומנט השני של הפקודה (רלוונטי רק ל-push ו-pop)
  def arg2: Int = {
    // מניחים שקראו לפונקציה הזו רק עבור פקודות שיש להן ארגומנט שני
    // לוקחים את המילה השלישית וממירים אותה למספר שלם
    currentCommand.split(" ")(2).toInt
  }
}