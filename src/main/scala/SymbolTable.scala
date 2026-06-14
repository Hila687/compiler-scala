// Holds information about a single symbol (variable).
// The variable name itself is used as the key in the maps.
case class SymbolInfo(
                       typeName: String,
                       kind: String,
                       index: Int
                     )

class SymbolTable {

  // Symbols that belong to the class scope:
  // static and field variables.
  private var classScope = Map[String, SymbolInfo]()

  // Symbols that belong to the current subroutine scope:
  // arguments and local variables.
  private var subroutineScope = Map[String, SymbolInfo]()

  // Running counters for assigning indices.
  private var staticCount = 0
  private var fieldCount = 0
  private var argumentCount = 0
  private var varCountNum = 0

  // Starts a new subroutine scope.
  // Class-level symbols remain unchanged.
  def startSubroutine(): Unit = {
    subroutineScope = Map()
    argumentCount = 0
    varCountNum = 0
  }

  // Defines a new symbol and assigns it the next available index
  // according to its kind.
  def define(name: String, typeName: String, kind: String): Unit = {

    val index = kind match {

      case "static" =>
        val i = staticCount
        staticCount += 1
        i

      case "field" =>
        val i = fieldCount
        fieldCount += 1
        i

      case "argument" =>
        val i = argumentCount
        argumentCount += 1
        i

      case "var" =>
        val i = varCountNum
        varCountNum += 1
        i

      case _ =>
        throw new IllegalArgumentException(
          s"Unknown kind: $kind"
        )
    }

    val info = SymbolInfo(typeName, kind, index)

    kind match {

      case "static" | "field" =>
        classScope += name -> info

      case "argument" | "var" =>
        subroutineScope += name -> info
    }
  }

  // Returns the number of variables already defined
  // for the given kind.
  def varCount(kind: String): Int = kind match {
    case "static" => staticCount
    case "field" => fieldCount
    case "argument" => argumentCount
    case "var" => varCountNum
    case _ => 0
  }

  // Checks whether a symbol exists in either scope.
  def contains(name: String): Boolean = {
    subroutineScope.contains(name) ||
      classScope.contains(name)
  }

  // Searches first in the subroutine scope
  // and then in the class scope.
  private def find(name: String): Option[SymbolInfo] = {
    subroutineScope
      .get(name)
      .orElse(classScope.get(name))
  }

  // Returns the kind of the symbol.
  def kindOf(name: String): String = {
    find(name)
      .map(_.kind)
      .getOrElse("NONE")
  }

  // Returns the declared type of the symbol.
  def typeOf(name: String): String = {
    find(name)
      .map(_.typeName)
      .getOrElse("NONE")
  }

  // Returns the running index of the symbol.
  def indexOf(name: String): Int = {
    find(name)
      .map(_.index)
      .getOrElse(-1)
  }
}