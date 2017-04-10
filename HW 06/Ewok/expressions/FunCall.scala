package expressions
import values._
import ui._

case class FunCall(val operator: Identifier, operands: List[Expression]) extends Expression {

  def execute(env: Environment) = {
    system.execute(operator, operands.map(_.execute(env)))
  }
}