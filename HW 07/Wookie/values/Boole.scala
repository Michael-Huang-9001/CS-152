package values
import expressions._
import ui._

case class Boole(val value: Boolean) extends Literal {

  def &&(other: Boole) =
    if (this.value && other.value)
      new Boole(true)
    else
      new Boole(false)

  def ||(other: Boole) =
    if (this.value == true)
      new Boole(true)
    else if (other.value == true)
      new Boole(true)
    else
      new Boole(false)

  def !() = new Boole(!value)

  override def toString = "" + value
}

object Boole {
  def test = {
    val a = Boole(true)
    val b = Boole(false)
    val c = a && b
    val d = (a && b) || (a || b)
    println(c)
    println(d)
  }
}