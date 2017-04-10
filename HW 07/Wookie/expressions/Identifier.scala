package expressions
import values._
import ui._

case class Identifier(val name: String) extends Expression {
  def execute(env: Environment) = {
    env.find(this)
  }
}