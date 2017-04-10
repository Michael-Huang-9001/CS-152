package expressions
import values._
import ui._

case class Block(val locals: List[Expression]) extends SpecialForm {
  def execute(env: Environment) = {
    var tempEnv = new Environment(env)
    val args = locals.map(_.execute(tempEnv))
    args.last
  }
}