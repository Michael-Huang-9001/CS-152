package ui
import values._
import expressions._

import scala.util.parsing.combinator._

class EwokParsers_Old extends RegexParsers {
  //EXPRESSION ::= DECLARATION | CONDITIONAL | DISJUNCTION
  def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")

  // DECLARATION ::= def~IDENTIFIER~=~EXPRESSION 
  def declaration: Parser[Declaration] = "def" ~ identifier ~ "=" ~ expression ^^
    {
      case "def" ~ id ~ "=" ~ exp => Declaration(id, exp)
    }

  // CONDITIONAL ::= if~(~EXPRESSION~)~EXPRESSION~(else~EXPRESSION)?
  def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^
    {
      case "if" ~ "(" ~ condition ~ ")" ~ consequent ~ None                       => Conditional(condition, consequent)
      case "if" ~ "(" ~ condition ~ ")" ~ consequent ~ Some("else" ~ alternative) => Conditional(condition, consequent, alternative)
    }

  // DISJUNCTION ::= CONJUNCTION~(||~CONJUNCTION)*
  def disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^
    {
      case conj ~ Nil  => conj
      case conj ~ more => Disjunction(conj :: more)
    }

  // CONJUNCTION ::= EQUALITY~(&&~EQUALITY)*
  def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^
    {
      case eq ~ Nil  => eq
      case eq ~ more => Conjunction(eq :: more)
    }

  // EQUALITY ::= INEQUALITY~(==~INEQUALITY)*
  def equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^
    {
      case ineq ~ Nil  => ineq
      case ineq ~ more => FunCall(Identifier("equals"), ineq :: more)
    }

  // INEQUALITY ::= SUM~((<|>|<=|>=|!=)~SUM)*
  def inequality: Parser[Expression] = sum ~ rep(("<" | ">" | "<=" | ">=" | "!=") ~> sum) ^^
    {
      case sum ~ Nil => sum
      case sum ~ more   => FunCall(Identifier("less"), sum :: more)
    }

  // SUM ::= PRODUCT~((\+|-)~PRODUCT)*
  def sum: Parser[Expression] = product ~ rep(("+" | "-") ~ product ^^ { case "+" ~ s => s case "-" ~ s => negate(s) }) ^^ {
    case prod ~ Nil  => prod
    case prod ~ more => FunCall(Identifier("add"), prod :: more)
  }

  // PRODUCT ::= FUNCALL~((\*|/)~FUNCALL)*
  def product: Parser[Expression] = funcall ~ rep(("*" | "/") ~ funcall ^^ { case "*" ~ s => s case "/" ~ s => reciprocal(s) }) ^^ {
    case funcall ~ Nil  => funcall
    case funcall ~ more => FunCall(Identifier("mul"), funcall :: more)
  }

  // FUNCALL ::= TERM~OPERANDS?
  def funcall: Parser[Expression] = term ~ opt(operands) ^^
    {
      case term ~ None      => term
      case term ~ Some(Nil) => FunCall(term.asInstanceOf[Identifier], Nil)
      case term ~ Some(operands) => FunCall(term.asInstanceOf[Identifier], operands)
    }

  // OPERANDS ::= (~(EXPRESSION~(,~EXPRESSION)*)?)
  def operands: Parser[List[Expression]] = "(" ~> opt(expression ~ rep("," ~> expression)) <~ ")" ^^
    {
      case None             => Nil
      case Some(exp ~ Nil)  => List(exp)
      case Some(exp ~ more) => exp :: more
      case _                => Nil
    }

  // TERM ::= LITERAL | IDENTIFIER  | (~EXPRESSION~)
  def term: Parser[Expression] = literal | identifier | "(" ~> expression <~ ")"

  // LITERAL ::= BOOLE | NUMBER
  def literal: Parser[Literal] = boole | number

  // IDENTIFIER ::= [a-zA-Z][0-9a-zA-Z]*
  def identifier: Parser[Identifier] = """[a-zA-z][a-zA-Z0-9]*""".r ^^
    {
      case id => Identifier(id)
    }

  // BOOLE ::= true | false
  def boole: Parser[Boole] = """true|false""".r ^^
    {
      case boole => new Boole(boole.toBoolean)
    }

  // NUMBER ::= (\+|-)?[0-9]+(\.[0-9]+)?
  def number: Parser[Number] = """(\+|-)?[0-9]+(\.[0-9]+)?""".r ^^
    {
      case num => new Number(num.toDouble)
    }

  // NEGATE: This is for turning addition into subtraction.
  def negate(exp: Expression): Expression = {
    val sub = Identifier("sub")
    val zero = Number(0)
    FunCall(sub, List(zero, exp))
  }
  
  // RECIPROCAL: This is for turning multiplication into division.
  def reciprocal(exp: Expression): Expression = {
    val div = Identifier("div")
    val one = Number(1.0)
    FunCall(div, List(one, exp))
  }

  // def declaration, conditional, disjunction, and other parsers
}