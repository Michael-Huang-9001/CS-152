package values
import expressions._
import ui._

case class Number(val value: Double) extends Literal {
  def +(other: Number) = Number(this.value + other.value)
  def -(other: Number) = Number(this.value - other.value)
  def *(other: Number) = Number(this.value * other.value)
  def /(other: Number) = Number(this.value / other.value)
  def <(other: Number) = Boole(this.value < other.value)
  def >(other: Number) = Boole(this.value > other.value)
  def ==(other: Number) = Boole(this.value == other.value)
  def !=(other: Number) = Boole(this.value != other.value)
  override def toString = "" + value
}

object Number {
  def test = {
    val a = Number(39.0)
    val b = Number(101)
    val c = a+b
    val d = a < b
    val e = a > b
    val f = a != b
    println(c + " " + d + " " + e + " " + f)
  }
}