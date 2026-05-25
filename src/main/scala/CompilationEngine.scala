import java.io.PrintWriter

class CompilationEngine(tokenizer: JackTokenizer, writer: PrintWriter) {

  // פונקציית עזר: מדפיסה טוקן סופי (terminal) ומתקדמת לטוקן הבא
  private def processTerminal(): Unit = {
    if (tokenizer.tokenType.nonEmpty) {
      writer.println(tokenizer.getXmlString)
    }
    tokenizer.advance()
  }

  /**
   * מנתח מחלקה שלמה (class)
   * חוק הדקדוק: 'class' className '{' classVarDec* subroutineDec* '}'
   */
  def compileClass(): Unit = {
    writer.println("<class>")

    // 'class' className '{'
    processTerminal()
    processTerminal()
    processTerminal()

    // לולאה שמטפלת בכל משתני המחלקה (classVarDec*) כל עוד יש static או field
    while (tokenizer.tokenType == "keyword" && (tokenizer.getTokenValue == "static" || tokenizer.getTokenValue == "field")) {
      compileClassVarDec()
    }

    // לולאה שמטפלת בכל הפונקציות (subroutineDec*) כל עוד יש constructor, function או method
    while (tokenizer.tokenType == "keyword" &&
      (tokenizer.getTokenValue == "constructor" || tokenizer.getTokenValue == "function" || tokenizer.getTokenValue == "method")) {
      compileSubroutine()
    }

    // '}'
    processTerminal()

    writer.println("</class>")
  }

  /**
   * מנתח הצהרת משתני מחלקה (static / field)
   * חוק הדקדוק: ('static' | 'field') type varName (',' varName)* ';'
   */
  def compileClassVarDec(): Unit = {
    writer.println("<classVarDec>")

    // 'static' or 'field'
    processTerminal()
    // type (int, char, boolean, className)
    processTerminal()
    // varName
    processTerminal()

    // טיפול ברשימת משתנים המופרדים בפסיק ( , var2, var3)
    while (tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == ",") {
      processTerminal() // ','
      processTerminal() // varName
    }

    // ';'
    processTerminal()

    writer.println("</classVarDec>")
  }

  /**
   * מנתח הצהרת פונקציה, מתודה או בנאי
   * חוק הדקדוק: ('constructor' | 'function' | 'method') ('void' | type) subroutineName '(' parameterList ')' subroutineBody
   */
  def compileSubroutine(): Unit = {
    writer.println("<subroutineDec>")

    // 'constructor' | 'function' | 'method'
    processTerminal()
    // 'void' | type
    processTerminal()
    // subroutineName
    processTerminal()
    // '('
    processTerminal()

    // ניתוח רשימת הפרמטרים (תמיד מייצר תגית, גם אם ריקה!)
    compileParameterList()

    // ')'
    processTerminal()

    // ניתוח גוף הפונקציה
    compileSubroutineBody()

    writer.println("</subroutineDec>")
  }

  /**
   * מנתח רשימת פרמטרים של פונקציה (לא כולל הסוגריים)
   * חוק הדקדוק: ((type varName) (',' type varName)*)?
   */
  def compileParameterList(): Unit = {
    writer.println("<parameterList>")

    // אם הטוקן הנוכחי הוא לא ')', סימן שיש פרמטרים ברשימה
    if (!(tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == ")")) {
      // type
      processTerminal()
      // varName
      processTerminal()

      // אם יש עוד פרמטרים המופרדים בפסיק
      while (tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == ",") {
        processTerminal() // ','
        processTerminal() // type
        processTerminal() // varName
      }
    }

    writer.println("</parameterList>")
  }

  /**
   * מנתח את גוף הפונקציה (הבלוק הפנימי)
   * חוק הדקדוק: '{' varDec* statements '}'
   */
  def compileSubroutineBody(): Unit = {
    writer.println("<subroutineBody>")

    // '{'
    processTerminal()

    // לולאה שמטפלת בכל הצהרות המשתנים המקומיים (varDec*) כל עוד יש מילת מפתח var
    while (tokenizer.tokenType == "keyword" && tokenizer.getTokenValue == "var") {
      compileVarDec()
    }

    // קריאה לניתוח הפקודות בתוך גוף הפונקציה
    compileStatements()

    // '}'
    processTerminal()

    writer.println("</subroutineBody>")
  }

  /**
   * מנתח הצהרת משתנים מקומיים בתוך פונקציה
   * חוק הדקדוק: 'var' type varName (',' varName)* ';'
   */
  def compileVarDec(): Unit = {
    writer.println("<varDec>")

    // 'var'
    processTerminal()
    // type
    processTerminal()
    // varName
    processTerminal()

    // אם יש עוד משתנים באותה שורה המופרדים בפסיק
    while (tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == ",") {
      processTerminal() // ','
      processTerminal() // varName
    }

    // ';'
    processTerminal()

    writer.println("</varDec>")
  }

  /**
   * מנתח סדרה של פקודות (statements)
   * חוק הדקדוק: statement*
   */
  def compileStatements(): Unit = {
    writer.println("<statements>")

    // הלולאה רצה כל עוד אנחנו פוגשים מילות מפתח של פקודות חוקיות
    while (tokenizer.tokenType == "keyword" &&
      (tokenizer.getTokenValue == "let" || tokenizer.getTokenValue == "if" ||
        tokenizer.getTokenValue == "while" || tokenizer.getTokenValue == "do" ||
        tokenizer.getTokenValue == "return")) {

      tokenizer.getTokenValue match {
        case "let" => compileLet()
        case "if" => compileIf()
        case "while" => compileWhile()
        case "do" => compileDo()
        case "return" => compileReturn()
      }
    }

    writer.println("</statements>")
  }

  def compileLet(): Unit = {
    writer.println("<letStatement>")
    processTerminal() // 'let'
    processTerminal() // varName

    // אם מדובר בהשמה למערך: '[' expression ']'
    if (tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == "[") {
      processTerminal() // '['
      compileExpression()
      processTerminal() // ']'
    }

    processTerminal() // '='
    compileExpression()
    processTerminal() // ';'
    writer.println("</letStatement>")
  }

  def compileIf(): Unit = {
    writer.println("<ifStatement>")
    processTerminal() // 'if'
    processTerminal() // '('
    compileExpression()
    processTerminal() // ')'
    processTerminal() // '{'
    compileStatements()
    processTerminal() // '}'

    // טיפול בבלוק else אופציונלי
    if (tokenizer.tokenType == "keyword" && tokenizer.getTokenValue == "else") {
      processTerminal() // 'else'
      processTerminal() // '{'
      compileStatements()
      processTerminal() // '}'
    }
    writer.println("</ifStatement>")
  }

  def compileWhile(): Unit = {
    writer.println("<whileStatement>")
    processTerminal() // 'while'
    processTerminal() // '('
    compileExpression()
    processTerminal() // ')'
    processTerminal() // '{'
    compileStatements()
    processTerminal() // '}'
    writer.println("</whileStatement>")
  }

  def compileDo(): Unit = {
    writer.println("<doStatement>")
    processTerminal() // 'do'

    // קריאה לפונקציה/מתודה
    processTerminal() // שם הפונקציה או שם המחלקה/אובייקט

    if (tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == ".") {
      processTerminal() // '.'
      processTerminal() // שם המתודה הפנימית
    }

    processTerminal() // '('
    compileExpressionList()
    processTerminal() // ')'
    processTerminal() // ';'
    writer.println("</doStatement>")
  }

  def compileReturn(): Unit = {
    writer.println("<returnStatement>")
    processTerminal() // 'return'

    // אם הצעד הבא הוא לא ';', סימן שהפונקציה מחזירה ערך (expression)
    if (!(tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == ";")) {
      compileExpression()
    }

    processTerminal() // ';'
    writer.println("</returnStatement>")
  }

  /**
   * מנתח ביטוי (expression)
   * חוק הדקדוק: term (op term)*
   */
  def compileExpression(): Unit = {
    writer.println("<expression>")
    compileTerm()

    // כל עוד יש אופרטור בינארי חוקי, נמשיך לאסוף אותו ואת האיבר הבא
    val opSet = Set("+", "-", "*", "/", "&", "|", "<", ">", "=")
    while (tokenizer.tokenType == "symbol" && opSet.contains(tokenizer.getTokenValue)) {
      processTerminal() // האופרטור (למשל +)
      compileTerm()     // האיבר הבא (term)
    }

    writer.println("</expression>")
  }

  /**
   * מנתח איבר בודד בתוך ביטוי (term)
   */
  def compileTerm(): Unit = {
    writer.println("<term>")

    // קבועים בסיסיים: מספר, מחרוזת או מילים שמורות (true, false, null, this)
    if (tokenizer.tokenType == "integerConstant" || tokenizer.tokenType == "stringConstant" || tokenizer.tokenType == "keyword") {
      processTerminal()
    }
    // מזהים (משתנים, מערכים או קריאות לפונקציות)
    else if (tokenizer.tokenType == "identifier") {
      processTerminal() // שם המזהה

      // א. מערך: varName[expression]
      if (tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == "[") {
        processTerminal() // '['
        compileExpression()
        processTerminal() // ']'
      }
      // ב. קריאה ישירה לפונקציה: subroutineName(arguments)
      else if (tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == "(") {
        processTerminal() // '('
        compileExpressionList()
        processTerminal() // ')'
      }
      // ג. קריאה דרך אובייקט/מחלקה: className.methodName(arguments)
      else if (tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == ".") {
        processTerminal() // '.'
        processTerminal() // methodName
        processTerminal() // '('
        compileExpressionList()
        processTerminal() // ')'
      }
    }
    // אופרטור אונרי (כמו מינוס לפני מספר או סימן שלילת לוגיקה ~)
    else if (tokenizer.tokenType == "symbol" && (tokenizer.getTokenValue == "-" || tokenizer.getTokenValue == "~")) {
      processTerminal()
      compileTerm()
    }
    // ביטוי פנימי עטוף בסוגריים: '(' expression ')'
    else if (tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == "(") {
      processTerminal() // '('
      compileExpression()
      processTerminal() // ')'
    }

    writer.println("</term>")
  }

  /**
   * מנתח רשימת ביטויים המופרדים בפסיקים (עבור ארגומנטים של פונקציות)
   * חוק הדקדוק: (expression (I ',' expression)*)?
   */
  def compileExpressionList(): Unit = {
    writer.println("<expressionList>")

    // הרשימה מסתיימת כשפוגשים סוגר ימני ')'
    if (!(tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == ")")) {
      compileExpression()

      while (tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == ",") {
        processTerminal() // ','
        compileExpression()
      }
    }

    writer.println("</expressionList>")
  }
}