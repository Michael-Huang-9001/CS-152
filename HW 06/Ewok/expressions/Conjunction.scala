package expressions
import values._
import ui._

case class Conjunction(val operands: List[Expression]) extends SpecialForm {
  def execute(env: Environment) = {
    var result = true
    for (i <- operands; if(result)) {
      var boole = i.execute(env)
      if (!boole.isInstanceOf[Boole])
        throw new TypeException("a conjunction only takes Boole")
      result = boole.asInstanceOf[Boole].value
    }
    new Boole(result)
  }
}