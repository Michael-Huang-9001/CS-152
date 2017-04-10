package ui

import values._
import expressions._

object system {
  def execute(opcode: Identifier, args: List[Value]): Value = {
    opcode.name match {
      case "add"    => add(args)
      case "sub"    => sub(args)
      case "mul"    => mul(args)
      case "div"    => div(args)
      case "less"   => less(args)
      case "more"   => more(args)
      case "equals" => equals(args)
      case "unequals" => unequals(args)
      case "not"    => not(args)
      case _        => throw new UndefinedException(opcode.name)
    }
  }

  private def add(vals: List[Value]): Value = {
    if (vals.isEmpty)
      throw new TypeException("addition expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length)
      throw new TypeException("all addition inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_ + _)
  }

  private def sub(vals: List[Value]): Value = {
    if (vals.isEmpty)
      throw new TypeException("subtraction expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length)
      throw new TypeException("all subtraction inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_ - _)
  }

  private def mul(vals: List[Value]): Value = {
    if (vals.isEmpty)
      throw new TypeException("multiplication expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length)
      throw new TypeException("all multiplication inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_ * _)
  }

  private def div(vals: List[Value]): Value = {
    if (vals.isEmpty)
      throw new TypeException("division expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length)
      throw new TypeException("all division inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_ / _)
  }

  private def less(vals: List[Value]): Value = {
    if (vals.size != 2)
      throw new TypeException("inequality expects 2 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length)
      throw new TypeException("all inequality inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    // vals.head < less(vals.tail)
    args2(0) < args2(1)
    
  }

  private def more(vals: List[Value]): Value = {
    if (vals.size != 2)
      throw new TypeException("inequality expects 2 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length)
      throw new TypeException("all inequality inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    // args2.reduce(_ > _)
    args2(0) > args2(1)
  }

  private def equals(vals: List[Value]): Value = {
    if (vals.size != 2)
      throw new TypeException("equals expect 2 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length)
      throw new TypeException("all equals inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    // args2.reduce(_ == _)
    args2(0) == args2(1)
  }
  
  private def unequals(vals: List[Value]): Value = {
    if (vals.size != 2)
      throw new TypeException("unequals expect 2 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length)
      throw new TypeException("all unequals inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2(0) != args2(1)
  }

  private def not(vals: List[Value]): Value = {
    if (vals.length != 1)
      throw new TypeException("not expects 1 input")
    val ok = vals.filter(_.isInstanceOf[Boole])
    if (ok.length < vals.length)
      throw new TypeException("the input must be a Boole")
    val args2 = vals.map(_.asInstanceOf[Boole])
    args2(0)!
  }

  // etc.
}