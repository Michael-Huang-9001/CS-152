package ui
import scala.util.parsing.combinator._

class TypeException(val msg: String) extends JediException("Type error: " + msg) {}