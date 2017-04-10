package expressions
import values._
import ui._

case class Conditional(condition: Expression, consequence: Expression, alternative: Expression = null) extends SpecialForm {
  def execute(env: Environment) = {
    condition.execute(env) match {
      case Boole(value) => {
        if (value)
          consequence.execute(env)
        else if (alternative != null)
          alternative.execute(env)
        else
          Notification.UNKNOWN
      }
      case _ => throw new TypeException("a condition only takes Boole")
    }
  }
}