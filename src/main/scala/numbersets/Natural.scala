package numbersets

import scala.util.Try

/**
  * Exception thrown in case a subtraction leads to a non-natural number
  */
case class NotANaturalNumber() extends Exception("Not a natural number")

/**
  * Trait for natural numbers
  */
trait Natural extends Ordered[Natural] {
  def +(x:Natural):Natural
  def -(x:Natural):Natural
  def *(x:Natural):Natural

  def /(x: Natural): Natural

  def %(x: Natural): Natural
  override def compare(that: Natural): Int = Try(this-that).getOrElse(return -1) match {
    case Zero => 0
    case x : NaturalNumber => 1
  }
  def successor:Natural = new NaturalNumber(this)
  override def toString = toInt toString
  implicit def toInt : Int
}

/**
  * Representation of 0 as a natural number
  */
object Zero extends Natural {

  def +(x:Natural) = x

  def -(x:Natural) = x match {
    case Zero => Zero
    case _ => throw NotANaturalNumber()
  }
  def *(x:Natural) = Zero

  def %(x: Natural) = Zero

  implicit def toInt = 0

  def /(x: Natural) = Zero
}

/**
  * Representation of natural numbers other than 0
  * @param predecessor predecessing number
  */
class NaturalNumber(private val predecessor:Natural) extends Natural {

  def +(x:Natural) = this.predecessor+x.successor

  def *(x:Natural) = {
    def product(acc:Natural,n:Natural):Natural = n match {
        case Zero => acc
        case n:NaturalNumber => product(this+acc, n.predecessor)
      }
    product(Zero,x)
  }

  def /(x: Natural) = {
    def div(remainder: Natural, acc: Natural): Natural = {
      if (remainder < x) acc
      else div(remainder - x, acc.successor)
    }
    if (x == Zero) throw NotANaturalNumber()
    div(this, Zero)
  }

  def %(x: Natural) = {
    def mod(acc: Natural): Natural = {
      if (acc < x) acc
      else mod(acc - x)
    }
    if (x == Zero) throw NotANaturalNumber()
    mod(this)
  }

  def -(x:Natural) = x match {
    case Zero => this
    case x : NaturalNumber => predecessor - x.predecessor
  }

  implicit def toInt = {
    def natVal(n: Natural, x: Int): Int = n match {
      case Zero => x
      case n: NaturalNumber => natVal(n.predecessor, x + 1)
    }
    natVal(this, 0)
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
    def sum(x:Int,n:Natural):Natural = x match {
      case 0 => n
      case _ => sum(x-1,n.successor)
    }
    sum(x,Zero)
  }
}