package ui
import values._
import expressions._

object console {
  val parsers = new EwokParsers // for now
  val globalEnv = new Environment

  def execute(cmmd: String): String = {
    val tree = parsers.parseAll(parsers.expression, cmmd)
    tree match {
      case t: parsers.Failure => throw new SyntaxException(t)
      case _ => {
        val exp = tree.get
        val result = exp.execute(globalEnv)
        result.toString
      }
    }
  }

  def repl {
    // declare locals
    var more = true

    while (more) {
      try {
        print("--> ")
        val cmmd = readLine()
        // read/execute/print
        if (cmmd.equals("quit")) {
          println("Bye")
          more = false
        } else {
          println(execute(cmmd))
        }
      } catch {
        case s: SyntaxException => {
          println(s.gripe)
          println(s.result.msg)
          println("line # = " + s.result.next.pos.line)
          println("column # = " + s.result.next.pos.column)
          println("token = " + s.result.next.first)
        }
        case u: UndefinedException => println("Unidentified identifier: " + u.msg)
        case t: TypeException      => println("TypeException: " + t.msg)
        case _: Throwable          => println("Other error. Something went wrong.")
        // handle other types of exceptions
      } finally {
        Console.flush
      }
    }
  }

  def main(args: Array[String]): Unit = { repl }
}