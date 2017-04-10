package ui

case class UndefinedException(val msg: String) extends JediException("Undefined Identifier: " + msg) {}