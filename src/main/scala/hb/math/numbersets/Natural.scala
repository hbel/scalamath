package hb.math.numbersets

import scala.util.Try

/**
  * Implicit conversion between Ints and Natural numbers
  */
object NaturalImplicits {
  implicit def fromInt(i: Int): Natural = Natural(i)

  implicit def toInt(nat: Natural): Int = {
    def natVal(n: Natural, x: Int): Int = n match {
      case Zero => x
      case NaturalNumber(pred) => natVal(pred, x + 1)
    }
    natVal(nat, 0)
  }
}

/**
  * Exception thrown in case a subtraction leads to a non-natural number
  */
case class NotANaturalNumber() extends Exception("Not a natural number")

/**
  * Trait for natural numbers
  */
trait Natural extends Ordered[Natural] {
  def +(that: Natural): Natural

  def -(that: Natural): Natural

  def *(that: Natural): Natural

  def /(that: Natural): Natural

  def %(that: Natural): Natural

  override def compare(that: Natural): Int = Try(this-that).getOrElse(return -1) match {
    case Zero => 0
    case x : NaturalNumber => 1
  }

  def successor: Natural = NaturalNumber(this)

  override def toString = NaturalImplicits.toInt(this) toString
}

/**
  * Representation of 0 as a natural number
  */
object Zero extends Natural {

  def +(that: Natural) = that

  def -(that: Natural) = that match {
    case Zero => Zero
    case _ => throw NotANaturalNumber()
  }

  def *(that: Natural) = Zero

  def %(that: Natural) = Zero

  def /(that: Natural) = Zero
}

/**
  * Representation of natural numbers other than 0
  * @param predecessor predecessing number
  */
case class NaturalNumber(predecessor: Natural) extends Natural {

  def +(x: Natural) = this.predecessor + x.successor

  def *(x:Natural) = {
    def product(n: Natural, acc: Natural = Zero): Natural = n match {
      case Zero => acc
      case NaturalNumber(pred) => product(pred, this + acc)
    }
    product(x)
  }

  def /(that: Natural) = {
    def div(rest: Natural, acc: Natural): Natural = if (rest < that) acc else div(rest - that, acc.successor)

    if (that == Zero) throw NotANaturalNumber()
    div(this, Zero)
  }

  def %(that: Natural) = {
    def mod(acc: Natural): Natural = if (acc < that) acc else mod(acc - that)

    if (that == Zero) throw NotANaturalNumber()
    mod(this)
  }

  def -(that: Natural) = that match {
    case Zero => this
    case NaturalNumber(pred) => predecessor - pred
  }

  override def equals(o: scala.Any): Boolean = o match {
    case n : NaturalNumber => this.compare(n) == 0
    case _ => false
  }
}

/**
  * Natural number companion object
  */
object Natural {
  def apply(x:Int) = {
    def sum(x: Int, n: Natural = Zero): Natural = x match {
      case 0 => n
      case _ => sum(x-1,n.successor)
    }
    sum(x)
  }
}