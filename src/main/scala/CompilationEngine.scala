import java.io.PrintWriter
import java.io.StringWriter

class CompilationEngine(tokenizer: JackTokenizer, vmWriter: VMWriter) {

  private val symbolTable = new SymbolTable()

  private var className = ""
  private var currentSubroutineName = ""
  private var currentSubroutineType = ""

  // Temporary dummy writer.
  // This keeps the old XML-writing code compiling until the VM translation
  // of statements and expressions is implemented.
  private val writer = new PrintWriter(new StringWriter())

  // Consumes the current token and advances to the next one.
  // Returns the consumed token value, so the compiler can use it.
  private def processTerminal(): String = {
    val value = tokenizer.getTokenValue
    tokenizer.advance()
    value
  }

  /**
   * Compiles a complete Jack class.
   * Grammar: 'class' className '{' classVarDec* subroutineDec* '}'
   */
  def compileClass(): Unit = {

    processTerminal()              // 'class'
    className = processTerminal()  // className
    processTerminal()              // '{'

    while (
      tokenizer.tokenType == "keyword" &&
        (tokenizer.getTokenValue == "static" || tokenizer.getTokenValue == "field")
    ) {
      compileClassVarDec()
    }

    while (
      tokenizer.tokenType == "keyword" &&
        (
          tokenizer.getTokenValue == "constructor" ||
            tokenizer.getTokenValue == "function" ||
            tokenizer.getTokenValue == "method"
          )
    ) {
      compileSubroutine()
    }

    processTerminal()              // '}'
  }

  /**
   * Compiles class-level variable declarations.
   * Grammar: ('static' | 'field') type varName (',' varName)* ';'
   */
  def compileClassVarDec(): Unit = {

    val kind = processTerminal()      // 'static' or 'field'
    val typeName = processTerminal()  // type
    val name = processTerminal()      // varName

    symbolTable.define(name, typeName, kind)

    while (tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == ",") {
      processTerminal()              // ','
      val nextName = processTerminal()
      symbolTable.define(nextName, typeName, kind)
    }

    processTerminal()                // ';'
  }

  /**
   * Compiles a constructor, function, or method declaration.
   * Grammar:
   * ('constructor' | 'function' | 'method')
   * ('void' | type) subroutineName '(' parameterList ')' subroutineBody
   */
  def compileSubroutine(): Unit = {

    currentSubroutineType = processTerminal()  // constructor / function / method
    processTerminal()                          // return type
    currentSubroutineName = processTerminal()  // subroutineName

    // Each subroutine has its own argument/local scope.
    symbolTable.startSubroutine()

    // In Jack, every method receives 'this' as argument 0.
    if (currentSubroutineType == "method") {
      symbolTable.define("this", className, "argument")
    }

    processTerminal()      // '('
    compileParameterList()
    processTerminal()      // ')'

    compileSubroutineBody()
  }

  /**
   * Compiles the parameter list of a subroutine.
   * Grammar: ((type varName) (',' type varName)*)?
   */
  def compileParameterList(): Unit = {

    // Empty parameter list.
    if (tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == ")") {
      return
    }

    val firstType = processTerminal()  // type
    val firstName = processTerminal()  // varName
    symbolTable.define(firstName, firstType, "argument")

    while (tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == ",") {
      processTerminal()                // ','
      val typeName = processTerminal() // type
      val name = processTerminal()     // varName
      symbolTable.define(name, typeName, "argument")
    }
  }

  /**
   * Compiles the body of a subroutine.
   * Grammar: '{' varDec* statements '}'
   */
  def compileSubroutineBody(): Unit = {

    processTerminal() // '{'

    // First, collect all local variable declarations.
    while (tokenizer.tokenType == "keyword" && tokenizer.getTokenValue == "var") {
      compileVarDec()
    }

    val fullFunctionName = s"$className.$currentSubroutineName"
    val nLocals = symbolTable.varCount("var")

    // VM function declaration must be written after counting local variables.
    vmWriter.writeFunction(fullFunctionName, nLocals)

    currentSubroutineType match {

      case "constructor" =>
        val fieldCount = symbolTable.varCount("field")
        vmWriter.writePush("constant", fieldCount)
        vmWriter.writeCall("Memory.alloc", 1)
        vmWriter.writePop("pointer", 0)

      case "method" =>
        vmWriter.writePush("argument", 0)
        vmWriter.writePop("pointer", 0)

      case "function" =>
      // No special initialization is needed for regular functions.

      case _ =>
        throw new IllegalArgumentException(
          s"Unknown subroutine type: $currentSubroutineType"
        )
    }

    compileStatements()

    processTerminal() // '}'
  }

  /**
   * Compiles local variable declarations inside a subroutine.
   * Grammar: 'var' type varName (',' varName)* ';'
   */
  def compileVarDec(): Unit = {

    processTerminal()                // 'var'
    val typeName = processTerminal() // type
    val name = processTerminal()     // varName

    symbolTable.define(name, typeName, "var")

    while (tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == ",") {
      processTerminal()              // ','
      val nextName = processTerminal()
      symbolTable.define(nextName, typeName, "var")
    }

    processTerminal()                // ';'
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