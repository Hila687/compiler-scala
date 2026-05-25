import java.io.File
import scala.io.Source

class JackTokenizer(jackFile: File) {

  // 1. קריאת כל תוכן הקובץ למחרוזת אחת גדולה
  private val content: String = {
    val source = Source.fromFile(jackFile)
    val fileContent = source.mkString
    source.close()
    fileContent
  }

  // מצביע פנימי שיגיד לנו באיזה תו (אינדקס) אנחנו נמצאים כרגע בקובץ
  private var pointer: Int = 0

  // משתנים שיחזיקו את הכלים של ה-Token הנוכחי שמצאנו
  private var currentToken: String = ""
  private var currentTokenType: String = ""

  // 2. הגדרת קבוצות גלובליות של השפה (מילים שמורות וסימנים)
  private val keywords = Set(
    "class", "constructor", "function", "method", "field", "static", "var",
    "int", "char", "boolean", "void", "true", "false", "null", "this",
    "let", "do", "if", "else", "while", "return"
  )

  private val symbols = Set(
    '{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*', '/', '&', '|', '<', '>', '=', '~'
  )

  // 3. פונקציות עזר בסיסיות של ה-API

  // בודקת האם הגענו לסוף הקובץ
  def hasMoreTokens: Boolean = {
    pointer < content.length
  }

  // החזרת הסוג של הטוקן הנוכחי
  def tokenType: String = currentTokenType

  // החזרת הערך של הטוקן הנוכחי
  def getTokenValue: String = currentToken

  // פונקציית עזר לדילוג על רווחים והערות
  private def skipWhitespaceAndComments(): Unit = {
    var keepSkipping = true

    while (keepSkipping && pointer < content.length) {
      val currentChar = content(pointer)

      // 1. דילוג על רווחים, טאבים ושורות חדשות
      if (currentChar.isWhitespace) {
        pointer += 1
      }
      // 2. בדיקה האם מדובר בהערה (מתחיל בלוכסן)
      else if (currentChar == '/' && pointer + 1 < content.length) {
        val nextChar = content(pointer + 1)

        if (nextChar == '/') {
          // הערת שורה: "//" - רצים עד שמוצאים שורה חדשה
          pointer += 2
          while (pointer < content.length && content(pointer) != '\n') {
            pointer += 1
          }
        }
        else if (nextChar == '*') {
          // הערת בלוק: "/*" - רצים עד שמוצאים את הסגירה "*/"
          pointer += 2
          var foundEnd = false
          while (pointer + 1 < content.length && !foundEnd) {
            if (content(pointer) == '*' && content(pointer + 1) == '/') {
              foundEnd = true
              pointer += 2 // מדלגים גם על ה-*/ עצמם
            } else {
              pointer += 1
            }
          }
        }
        else {
          // זה רק סימן חילוק רגיל! מפסיקים לדלג כדי שלא נפספס אותו
          keepSkipping = false
        }
      }
      // אם זה לא רווח ולא הערה, הגענו לקוד אמיתי - מפסיקים לדלג
      else {
        keepSkipping = false
      }
    }
  }

  // מתקדמת לטוקן הבא ומעדכנת את currentToken ואת currentTokenType
  def advance(): Unit = {
    // איפוס המשתנים מהסיבוב הקודם!
    currentToken = ""
    currentTokenType = ""

    // 1. קודם כל מנקים את כל ה"זבל" (רווחים והערות) עד שמגיעים לקוד אמיתי
    skipWhitespaceAndComments()

    // אם הגענו לסוף הקובץ אחרי שניקינו את הרווחים, עוצרים
    if (!hasMoreTokens) {
      return
    }

    // 1. קודם כל מנקים את כל ה"זבל" (רווחים והערות) עד שמגיעים לקוד אמיתי
    skipWhitespaceAndComments()

    // אם הגענו לסוף הקובץ אחרי שניקינו את הרווחים, עוצרים
    if (!hasMoreTokens) {
      return
    }

    val c = content(pointer)

    // מקרה א': האם זה סימבול?
    if (symbols.contains(c)) {
      currentToken = c.toString
      currentTokenType = "symbol"
      pointer += 1
    }
    // מקרה ב': האם זו מחרוזת? (מתחילה בגרשיים)
    else if (c == '"') {
      pointer += 1 // מדלגים על הגרשיים הפותחים
      val start = pointer
      // רצים עד שמוצאים את הגרשיים הסוגרים
      while (pointer < content.length && content(pointer) != '"') {
        pointer += 1
      }
      currentToken = content.substring(start, pointer)
      currentTokenType = "stringConstant"
      pointer += 1 // מדלגים על הגרשיים הסוגרים
    }
    // מקרה ג': האם זה מספר? (מתחיל בספרה)
    else if (c.isDigit) {
      val start = pointer
      // רצים כל עוד אנחנו רואים ספרות
      while (pointer < content.length && content(pointer).isDigit) {
        pointer += 1
      }
      currentToken = content.substring(start, pointer)
      currentTokenType = "integerConstant"
    }
    // מקרה ד': האם זו מילה? (מתחילה באות או מקף תחתון)
    else if (c.isLetter || c == '_') {
      val start = pointer
      // רצים כל עוד יש אותיות, ספרות או מקף תחתון
      while (pointer < content.length && (content(pointer).isLetterOrDigit || content(pointer) == '_')) {
        pointer += 1
      }
      val word = content.substring(start, pointer)
      currentToken = word

      // בדיקה: האם המילה שמצאנו היא מילה שמורה או שם של משתנה/פונקציה?
      if (keywords.contains(word)) {
        currentTokenType = "keyword"
      } else {
        currentTokenType = "identifier"
      }
    }
    // מקרה קצה לגיבוי (ידלג על תווים לא חוקיים אם יש כאלה)
    else {
      pointer += 1
    }
  }

  // פונקציה שמחזירה את שורת ה-XML המוכנה להדפסה עבור ה-Token הנוכחי
  def getXmlString: String = {
    // טיפול בסימנים מיוחדים ששוברים XML
    val xmlValue = if (currentTokenType == "symbol") {
      currentToken match {
        case "<" => "&lt;"
        case ">" => "&gt;"
        case "\"" => "&quot;"
        case "&" => "&amp;"
        case _ => currentToken
      }
    } else {
      currentToken
    }

    // החזרת השורה המעוצבת
    s"<$currentTokenType> $xmlValue </$currentTokenType>"
  }
}

