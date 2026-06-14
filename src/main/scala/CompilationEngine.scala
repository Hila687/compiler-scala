import java.io.PrintWriter
import java.io.StringWriter

class CompilationEngine(
                         tokenizer: JackTokenizer,
                         writer: PrintWriter,
                         symbolTable: SymbolTable,
                         vmWriter: VMWriter
                       ) {

  private var labelCount: Int = 0
  private var whileLabelCount: Int = 0

  private var className = ""
  private var currentSubroutineName = ""
  private var currentSubroutineType = ""

  private def processTerminal(): String = {
    val value = tokenizer.getTokenValue
    tokenizer.advance()
    value
  }

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

  def compileClassVarDec(): Unit = {
    val kind = processTerminal()
    val typeName = processTerminal()
    val name = processTerminal()

    symbolTable.define(name, typeName, kind)

    while (tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == ",") {
      processTerminal()
      val nextName = processTerminal()
      symbolTable.define(nextName, typeName, kind)
    }

    processTerminal()
  }

  def compileSubroutine(): Unit = {
    currentSubroutineType = processTerminal()
    processTerminal()
    currentSubroutineName = processTerminal()

    symbolTable.startSubroutine()

    if (currentSubroutineType == "method") {
      symbolTable.define("this", className, "argument")
    }

    processTerminal()
    compileParameterList()
    processTerminal()

    compileSubroutineBody()
  }

  def compileParameterList(): Unit = {
    if (tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == ")") {
      return
    }

    val firstType = processTerminal()
    val firstName = processTerminal()
    symbolTable.define(firstName, firstType, "argument")

    while (tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == ",") {
      processTerminal()
      val typeName = processTerminal()
      val name = processTerminal()
      symbolTable.define(name, typeName, "argument")
    }
  }

  def compileSubroutineBody(): Unit = {
    processTerminal()

    while (tokenizer.tokenType == "keyword" && tokenizer.getTokenValue == "var") {
      compileVarDec()
    }

    val fullFunctionName = s"$className.$currentSubroutineName"
    val nLocals = symbolTable.varCount("var")

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
      // No special initialization is needed.

      case _ =>
        throw new IllegalArgumentException(
          s"Unknown subroutine type: $currentSubroutineType"
        )
    }

    compileStatements()

    processTerminal()
  }

  def compileVarDec(): Unit = {
    processTerminal()
    val typeName = processTerminal()
    val name = processTerminal()

    symbolTable.define(name, typeName, "var")

    while (tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == ",") {
      processTerminal()
      val nextName = processTerminal()
      symbolTable.define(nextName, typeName, "var")
    }

    processTerminal()
  }

  /**
   * מנתח סדרה של פקודות (statements)
   */
  def compileStatements(): Unit = {
    writer.println("<statements>")

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

  /**
   * מנתח פקודת השמה (let) - מתורגם ל-VM!
   */
  def compileLet(): Unit = {
    tokenizer.advance() // מדלגים על המילה 'let'
    val varName = tokenizer.getTokenValue
    tokenizer.advance() // מדלגים על שם המשתנה

    // שולפים מראש את הנתונים על המשתנה מטבלת הסימבולים
    val kind = symbolTable.kindOf(varName)
    val index = symbolTable.indexOf(varName)
    val segment = kind match {
      case "var" => "local"
      case "argument" => "argument"
      case "static" => "static"
      case "field" => "this"
      case _ => kind
    }

    var isArray = false

    // טיפול במערך: let arr[expression1] = ...
    if (tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == "[") {
      isArray = true
      tokenizer.advance() // מדלגים על '['

      compileExpression() // מחשב את האינדקס הפנימי (expression1)

      tokenizer.advance() // מדלגים על ']'

      // דוחפים את כתובת הבסיס של המערך למחסנית
      vmWriter.writePush(segment, index)
      // מחברים: כתובת בסיס + אינדקס = כתובת היעד המדויקת בזיכרון!
      vmWriter.writeArithmetic("add")
    }

    tokenizer.advance() // מדלגים על הסימן '='

    // מחשבים את הערך להשמה (הצד הימני של המשוואה)
    compileExpression()

    tokenizer.advance() // מדלגים על הסימן ';'

    if (isArray) {
      // אם זה מערך, משתמשים בטריק ה-THAT:
      vmWriter.writePop("temp", 0)    // 1. שומרים את הערך שחישבנו בצד
      vmWriter.writePop("pointer", 1) // 2. מכוונים את THAT לכתובת היעד
      vmWriter.writePush("temp", 0)   // 3. מחזירים את הערך מהצד
      vmWriter.writePop("that", 0)    // 4. שומרים אותו בתוך המערך
    } else {
      // השמה רגילה למשתנה
      vmWriter.writePop(segment, index)
    }
  }

  def compileIf(): Unit = {
    // מייצרים תוויות ייחודיות ל-IF הנוכחי ומקדמים את המונה
    val labelFalse = "IF_FALSE" + labelCount
    val labelEnd = "IF_END" + labelCount
    labelCount += 1

    tokenizer.advance() // מדלגים על 'if'
    tokenizer.advance() // מדלגים על '('

    // 1. מחשבים את התנאי (התוצאה תהיה בראש המחסנית)
    compileExpression()

    tokenizer.advance() // מדלגים על ')'

    // 2. הופכים את התוצאה ובודקים אם צריך לקפוץ החוצה
    vmWriter.writeArithmetic("not")
    vmWriter.writeIf(labelFalse) // אם התנאי הפוך (שקר), קופצים ל-labelFalse

    tokenizer.advance() // מדלגים על '{'

    // 3. מריצים את הפקודות של ה-if
    compileStatements()

    tokenizer.advance() // מדלגים על '}'

    // 4. מסיימים את ה-if וקופצים לסוף כדי לדלג על ה-else
    vmWriter.writeGoto(labelEnd)

    // 5. כאן מתחיל אזור ה-else (אם קפצנו לפה, סימן שהתנאי היה שקר)
    vmWriter.writeLabel(labelFalse)

    // בודקים אם בכלל יש 'else'
    if (tokenizer.tokenType == "keyword" && tokenizer.getTokenValue == "else") {
      tokenizer.advance() // מדלגים על 'else'
      tokenizer.advance() // מדלגים על '{'

      compileStatements() // מריצים את הפקודות של ה-else

      tokenizer.advance() // מדלגים על '}'
    }

    // 6. התווית של סוף הבלוק כולו
    vmWriter.writeLabel(labelEnd)
  }

  def compileWhile(): Unit = {
    // מייצרים תוויות ייחודיות ללולאה הנוכחית ומקדמים את המונה
    val labelExp = "WHILE_EXP" + whileLabelCount
    val labelEnd = "WHILE_END" + whileLabelCount
    whileLabelCount += 1

    // 1. מסמנים את נקודת ההתחלה של הלולאה
    vmWriter.writeLabel(labelExp)

    tokenizer.advance() // מדלגים על 'while'
    tokenizer.advance() // מדלגים על '('

    // 2. מחשבים את התנאי
    compileExpression()

    tokenizer.advance() // מדלגים על ')'

    // 3. הופכים את התוצאה ובודקים אם צריך לצאת מהלולאה
    vmWriter.writeArithmetic("not")
    vmWriter.writeIf(labelEnd)

    tokenizer.advance() // מדלגים על '{'

    // 4. מריצים את הפקודות שבתוך הלולאה
    compileStatements()

    tokenizer.advance() // מדלגים על '}'

    // 5. קופצים חזרה להתחלה כדי לבדוק את התנאי שוב
    vmWriter.writeGoto(labelExp)

    // 6. מסמנים את סוף הלולאה
    vmWriter.writeLabel(labelEnd)
  }

  def compileDo(): Unit = {
    tokenizer.advance() // מדלגים על 'do'

    // 1. שומרים את המילה הראשונה (יכול להיות שם פונקציה, שם מחלקה, או שם משתנה-אובייקט)
    val name1 = tokenizer.getTokenValue
    tokenizer.advance()

    var funcName = ""
    var nArgs = 0

    // בודקים אם יש נקודה '.' (למשל Output.print או game.run)
    if (tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == ".") {
      tokenizer.advance() // מדלגים על '.'
      val name2 = tokenizer.getTokenValue // שומרים את שם הפונקציה שאחרי הנקודה
      tokenizer.advance()

      // שואלים את טבלת הסימבולים: האם name1 הוא משתנה שאנחנו מכירים?
      val typeOfVar = symbolTable.typeOf(name1)
      val kindOfVar = symbolTable.kindOf(name1)

      // אם הוא משתנה, זה אומר שאנחנו מפעילים מתודה על אובייקט ספציפי
      if (kindOfVar != "none" && kindOfVar != null) {
        val segment = kindOfVar match {
          case "var" => "local"
          case "field" => "this"
          case other => other
        }
        // דוחפים את כתובת האובייקט למחסנית (זה בעצם הארגומנט הראשון - this!)
        vmWriter.writePush(segment, symbolTable.indexOf(name1))
        funcName = typeOfVar + "." + name2
        nArgs = 1 // יש לנו כבר ארגומנט אחד שדחפנו (האובייקט עצמו)
      } else {
        // אם הוא לא משתנה, סימן ש-name1 הוא פשוט שם של מחלקה (למשל Output)
        funcName = name1 + "." + name2
      }
    } else {
      // אם אין נקודה בכלל (למשל do draw()), זו קריאה למתודה פנימית באותה מחלקה
      vmWriter.writePush("pointer", 0) // דוחפים את this של האובייקט הנוכחי!
      funcName = className + "." + name1
      nArgs = 1
    }

    tokenizer.advance() // מדלגים על '('

    // 2. מחשבים ודוחפים את כל הפרמטרים שבין הסוגריים, ומוסיפים אותם לספירה
    nArgs += compileExpressionList()

    tokenizer.advance() // מדלגים על ')'
    tokenizer.advance() // מדלגים על ';'

    // 3. קוראים לפונקציה!
    vmWriter.writeCall(funcName, nArgs)

    // 4. פקודת do תמיד-תמיד מחייבת אותנו לזרוק את התוצאה לפח
    vmWriter.writePop("temp", 0)
  }

  def compileReturn(): Unit = {
    tokenizer.advance() // מדלגים על המילה 'return'

    // בודקים אם יש ביטוי להחזיר (כלומר, הטוקן הנוכחי הוא לא נקודה-פסיק)
    if (tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == ";") {
      // אם אין ביטוי, זאת פונקציית void. ב-VM פונקציית void תמיד מחזירה 0.
      vmWriter.writePush("constant", 0)
    } else {
      // אם יש ביטוי, מחשבים אותו (התוצאה תונח אוטומטית בראש המחסנית)
      compileExpression()
    }

    tokenizer.advance() // מדלגים על ';'

    // כותבים את פקודת החזרה של ה-VM
    vmWriter.writeReturn()
  }

  def compileExpression(): Unit = {
    // 1. מחשבים את האיבר הראשון ודוחפים למחסנית
    compileTerm()

    val opList = List("+", "-", "*", "/", "&", "|", "<", ">", "=")

    // כל עוד יש אופרטור, סימן שהביטוי ממשיך (למשל x + y + z)
    while (tokenizer.tokenType == "symbol" && opList.contains(tokenizer.getTokenValue)) {
      // 2. שומרים את האופרטור
      val op = tokenizer.getTokenValue
      tokenizer.advance() // מדלגים עליו

      // 3. מחשבים את האיבר הבא ודוחפים למחסנית
      compileTerm()

      // 4. מפעילים את הפעולה המתמטית על שני האיברים שבראש המחסנית
      op match {
        case "+" => vmWriter.writeArithmetic("add")
        case "-" => vmWriter.writeArithmetic("sub")
        case "*" => vmWriter.writeCall("Math.multiply", 2) // קריאת מערכת לכפל
        case "/" => vmWriter.writeCall("Math.divide", 2)   // קריאת מערכת לחילוק
        case "&" => vmWriter.writeArithmetic("and")
        case "|" => vmWriter.writeArithmetic("or")
        case "<" => vmWriter.writeArithmetic("lt")
        case ">" => vmWriter.writeArithmetic("gt")
        case "=" => vmWriter.writeArithmetic("eq")
        case _ => // לא אמור לקרות
      }
    }
  }

  def compileTerm(): Unit = {
    val tType = tokenizer.tokenType
    val tVal = tokenizer.getTokenValue

    if (tType == "integerConstant") {
      vmWriter.writePush("constant", tVal.toInt)
      tokenizer.advance()
    }
    else if (tType == "stringConstant") {
      val str = tVal
      vmWriter.writePush("constant", str.length)
      vmWriter.writeCall("String.new", 1) // יוצרים אובייקט מחרוזת חדש במערכת ההפעלה

      // דוחפים תו-תו (קוד ASCII) ומוסיפים למחרוזת
      for (i <- 0 until str.length) {
        vmWriter.writePush("constant", str.charAt(i).toInt)
        vmWriter.writeCall("String.appendChar", 2)
      }
      tokenizer.advance()
    }
    else if (tType == "keyword") {
      tVal match {
        case "true" =>
          // ב-Jack הערך האמיתי של true הוא מינוס 1 (היפוך ביטים של 0)
          vmWriter.writePush("constant", 0)
          vmWriter.writeArithmetic("not")
        case "false" | "null" =>
          vmWriter.writePush("constant", 0)
        case "this" =>
          vmWriter.writePush("pointer", 0)
        case _ =>
      }
      tokenizer.advance()
    }
    else if (tType == "identifier") {
      val name1 = tVal
      tokenizer.advance() // מקדמים כדי לראות מה יש אחרי השם

      // מציצים לטוקן הבא כדי להבין אם זה מערך, קריאה לפונקציה או משתנה רגיל
      val nextToken = if (tokenizer.tokenType != "") tokenizer.getTokenValue else ""

      if (nextToken == "[") {
        // --- 1. טיפול במערך (name1[expression]) ---
        val kind = symbolTable.kindOf(name1)
        val segment = kind match { case "var" => "local" case "field" => "this" case other => other }
        vmWriter.writePush(segment, symbolTable.indexOf(name1)) // דוחפים כתובת בסיס

        tokenizer.advance() // מדלגים על '['
        compileExpression() // מחשבים את האינדקס
        tokenizer.advance() // מדלגים על ']'

        vmWriter.writeArithmetic("add") // מחברים: כתובת בסיס + אינדקס
        vmWriter.writePop("pointer", 1) // מכוונים את THAT לכתובת היעד
        vmWriter.writePush("that", 0)   // שולפים את הערך מתוך המערך אל המחסנית!
      }
      else if (nextToken == "(" || nextToken == ".") {
        // --- 2. טיפול בקריאה לפונקציה (name1.func() או name1()) ---
        var funcName = ""
        var nArgs = 0

        if (nextToken == ".") {
          tokenizer.advance() // מדלגים על '.'
          val name2 = tokenizer.getTokenValue
          tokenizer.advance() // מדלגים על שם הפונקציה

          val kindOfVar = symbolTable.kindOf(name1)
          val typeOfVar = symbolTable.typeOf(name1)

          if (kindOfVar != "none" && kindOfVar != null) {
            val segment = kindOfVar match { case "var" => "local" case "field" => "this" case other => other }
            vmWriter.writePush(segment, symbolTable.indexOf(name1))
            funcName = typeOfVar + "." + name2
            nArgs = 1
          } else {
            funcName = name1 + "." + name2
          }
        } else {
          vmWriter.writePush("pointer", 0)
          funcName = className + "." + name1
          nArgs = 1
        }

        tokenizer.advance() // מדלגים על '('
        nArgs += compileExpressionList()
        tokenizer.advance() // מדלגים על ')'

        vmWriter.writeCall(funcName, nArgs)
      }
      else {
        // --- 3. טיפול במשתנה רגיל ---
        val kind = symbolTable.kindOf(name1)
        val segment = kind match { case "var" => "local" case "field" => "this" case other => other }
        vmWriter.writePush(segment, symbolTable.indexOf(name1))
      }
    }
    else if (tType == "symbol" && (tVal == "-" || tVal == "~")) {
      // --- טיפול באופרטור לפני איבר (למשל מינוס) ---
      val op = tVal
      tokenizer.advance()
      compileTerm() // קודם מחשבים את האיבר

      if (op == "-") vmWriter.writeArithmetic("neg")
      else vmWriter.writeArithmetic("not")
    }
    else if (tType == "symbol" && tVal == "(") {
      // --- טיפול בסוגריים ---
      tokenizer.advance() // '('
      compileExpression()
      tokenizer.advance() // ')'
    }
  }

  def compileExpressionList(): Int = {
    var nArgs = 0 // מונה ארגומנטים

    // אם הרשימה לא ריקה (הצעד הבא הוא לא סוגר ימני)
    if (!(tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == ")")) {
      compileExpression()
      nArgs += 1

      // כל עוד יש פסיקים, ממשיכים לחשב את הביטויים הבאים
      while (tokenizer.tokenType == "symbol" && tokenizer.getTokenValue == ",") {
        tokenizer.advance() // מדלגים על הפסיק (במקום ה-processTerminal שהיה)
        compileExpression()
        nArgs += 1
      }
    }

    nArgs // מחזירים את מספר הארגומנטים שנספרו
  }
}