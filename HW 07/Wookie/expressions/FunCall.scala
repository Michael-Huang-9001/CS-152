package expressions
import values._
import ui._

case class FunCall(val operator: Expression, val operands: List[Expression] = Nil) extends Expression {

  def execute(env: Environment): Value = {
    val args: List[Value] = operands.map(_.execute(env))
    try {
      val opEx = operator.execute(env)
      if (opEx.isInstanceOf[Closure])
        opEx.asInstanceOf[Closure].apply(args)
      else
        throw new UndefinedException("It is not a Closure.")
    } catch {
      case e: UndefinedException => {
        system.execute(operator.asInstanceOf[Identifier], args)
      }
    }
  }
}