package values
import expressions._
import ui._

import scala.collection.mutable.HashMap

class Environment(var nextEnv: Environment = null) extends HashMap[Identifier, Value] with Value {

  def find(id: Identifier): Value = {
    if (this.contains(id))
      this(id)
    else if (nextEnv != null)
      nextEnv.find(id)
    else
      throw new UndefinedException(id.name)
  }

  def put(ids: List[Identifier], values: List[Value]) {
    for (i <- 0 until Math.min(ids.length, values.length)) {
      put(ids(i), values(i))
    }
  }
}