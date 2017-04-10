package expressions
import values._

trait Literal extends Value with Expression {
  def execute(env: Environment) = this
}