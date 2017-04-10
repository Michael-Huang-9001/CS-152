package ui

import scala.util.parsing.combinator._
import expressions._
import values._

// Most of parser provided by the professor.
class WookieParsers extends RegexParsers {

  def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")

  def declaration: Parser[Expression] = "def" ~ identifier ~ "=" ~ expression ^^
    {
      case "def" ~ id ~ "=" ~ exp => Declaration(id, exp)
    }

  def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^
    {
      case "if" ~ "(" ~ e1 ~ ")" ~ e2 ~ None              => Conditional(e1, e2)
      case "if" ~ "(" ~ e1 ~ ")" ~ e2 ~ Some("else" ~ e3) => Conditional(e1, e2, e3)
    }

  // DISJUNCTION ::= CONJUNCTION ~ (|| ~ CONJUNCTION)*
  def disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^
    {
      case conj ~ Nil  => conj
      case conj ~ more => Disjunction(conj :: more)
    }

  // CONJUNCTION ::= EQUALITY ~ (&& ~ EQUALITY)*
  def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^
    {
      case eq ~ Nil  => eq
      case eq ~ more => Conjunction(eq :: more)
    }

  // EQUALITY ::= INEQUALITY ~ (== ~ INEQUALITY)*
  def equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^
    {
      case ineq ~ Nil  => ineq
      case ineq ~ more => FunCall(Identifier("equals"), ineq :: more)
    }

  // INEQUALITY ::= SUM ~ ((< | > | !=) ~ SUM)?
  def inequality: Parser[Expression] = sum ~ opt(("<" | ">" | "!=") ~ sum) ^^
    {
      case sum ~ None              => sum
      case sum ~ Some("<" ~ more)  => FunCall(Identifier("less"), List(sum, more))
      case sum ~ Some(">" ~ more)  => FunCall(Identifier("more"), List(sum, more))
      case sum ~ Some("!=" ~ more) => FunCall(Identifier("unequals"), List(sum, more))
    }

  // exp -> 0 - exp
  def negate(exp: Expression): Expression = {
    val sub = Identifier("sub")
    val zero = Number(0)
    FunCall(sub, List(zero, exp))
  }

  // SUM ::= PRODUCT ~ ((\+|-) ~ PRODUCT)*
  def sum: Parser[Expression] = product ~ rep(("+" | "-") ~ product ^^ { case "+" ~ s => s case "-" ~ s => negate(s) }) ^^
    {
      case p ~ Nil  => p
      case p ~ rest => FunCall(Identifier("add"), p :: rest)
    }

  // exp => 1/exp
  def invert(exp: Expression): Expression = {
    val div = Identifier("div")
    val one = Number(1)
    FunCall(div, List(one, exp))
  }

  // RECIPROCAL: This is for turning multiplication into division.
  def reciprocal(exp: Expression): Expression = {
    val div = Identifier("div")
    val one = Number(1.0)
    FunCall(div, List(one, exp))
  }

  // PRODUCT ::= TERM ~ ((\* | /)~ TERM)*
  def product: Parser[Expression] = term ~ rep(("*" | "/") ~ term ^^ { case "*" ~ s => s case "/" ~ s => reciprocal(s) }) ^^ {
    case term ~ Nil  => term
    case term ~ more => FunCall(Identifier("mul"), term :: more)
  }

  // TERM ::= LITERAL | FUNCALL | IDENTIFIER | (~EXPRESSION~)
  def term: Parser[Expression] = block | lambda | literal | funcall | identifier | "(" ~> expression <~ ")"

  // OPERANDS ::= (~(EXPRESSION ~ (,~EXPRESSION)*)?~)
  // i.e. OPERANDS ::= A comma-separated list of 0 or more expressions bracketed by parentheses
  def operands: Parser[List[Expression]] = "(" ~> opt(expression ~ rep("," ~> expression)) <~ ")" ^^
    {
      case None           => Nil
      case Some(e ~ Nil)  => List(e)
      case Some(e ~ exps) => e :: exps
      case _              => Nil
    }

  // FUNCALL ::= IDENTIFIER ~ OPERANDS
  def funcall: Parser[Expression] = (lambda | identifier | "(" ~> expression <~ ")") ~ opt(operands) ^^
    {
      case e ~ None           => e
      case e ~ Some(operands) => FunCall(e, operands)
      // case e ~ operands       => FunCall(e, operands)
    }

  // LITERAL ::= BOOLE | NUMERAL
  def literal: Parser[Literal] = boole | numeral

  // NUMERAL ::= (\+|-)?~DIGIT~(.~DIGIT+)?
  def numeral: Parser[Number] = """(\+|-)?[0-9]+(\.[0-9]+)?""".r ^^
    {
      case e => Number(e.toDouble)
    }

  // BOOLE ::= true | false
  def boole: Parser[Boole] = """true|false|!true|!false""".r ^^
    {
      case "true"   => Boole(true)
      case "!true"  => Boole(false)
      case "false"  => Boole(false)
      case "!false" => Boole(true)
    }

  // IDENTIFIER ::= LETTER~(LETTER | DIGIT)*
  def identifier: Parser[Identifier] = """[a-zA-z][a-zA-Z0-9]*""".r ^^
    {
      case id => Identifier(id)
    }

  def lambda: Parser[Expression] = "lambda" ~ parameters ~ expression ^^ {
    case "lambda" ~ params ~ exp => new Lambda(params, exp)
  }

  def parameters: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^
    {
      case None           => Nil
      case Some(e ~ Nil)  => List(e)
      case Some(e ~ exps) => e :: exps
      // case _              => Nil
    }

  def block: Parser[Expression] = "{" ~ expression ~ rep(";" ~> expression) ~ "}" ^^ {
    case "{" ~ exp ~ Nil ~ "}"  => Block(List(exp))
    case "{" ~ exp ~ more ~ "}" => Block(exp :: more)
  }
}