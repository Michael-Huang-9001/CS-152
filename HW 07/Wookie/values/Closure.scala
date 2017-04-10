package values
import ui._
import expressions._

class Closure(params: List[Identifier], body: Expression, defEnv: Environment) extends Value {
  def apply(args: List[Value]): Value = {
    if (params.length != args.length)
      throw new TypeException("number of parameters and arguments do not match")
    var tempEnv = new Environment(defEnv)
    tempEnv.put(params, args)
    body.execute(tempEnv)
  }
}