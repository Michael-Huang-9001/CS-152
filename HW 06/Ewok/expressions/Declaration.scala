package expressions
import ui._
import values._

case class Declaration(id: Identifier, exp: Expression) extends SpecialForm {
  
  def execute(env: Environment) = { 
    env.put(id, exp.execute(env))
    Notification.OK
  }
}