package numbersets

import scala.util.Try

case class NotANaturalNumber() extends Exception("Not a natural number")

/**
  * Created by hendrik on 12.11.16.
  */
trait Natural {
  def +(x:Natural):Natural
  def -(x:Natural):Natural
  def *(x:Natural):Natural
  def >(x:Natural):Boolean = Try(this-x).getOrElse(Zero) match {
    case Zero => false
    case _ => true
  }
  def <(x:Natural):Boolean = Try(x-this).getOrElse(Zero) match {
    case Zero => false
    case _ => true
  }
  def successor:Natural = new NaturalNumber(this)
}

object Zero extends Natural {
  def +(x:Natural) = x
  def -(x:Natural) = x match {
    case Zero => Zero
    case _ => throw NotANaturalNumber()
  }
  def *(x:Natural) = Zero
  override def toString = "0"
}

class NaturalNumber(private val predecessor:Natural) extends Natural {
  def +(x:Natural) = this.predecessor+x.successor
  def *(x:Natural) = {
    def product(acc:Natural,n:Natural):Natural = n match {
        case Zero => acc
        case n:NaturalNumber => product(this+acc, n.predecessor)
      }
    product(Zero,x)
  }
  def -(x:Natural) = x match {
    case Zero => this
    case x : NaturalNumber => predecessor - x.predecessor
    case _ => throw NotANaturalNumber()
  }

  override def equals(o: scala.Any): Boolean = o match {
    case n : NaturalNumber => n.predecessor == this.predecessor
    case _ => false
  }
  override def toString: String = {
    def natVal(n:Natural,x:Int):Int = n match {
      case Zero => x
      case n : NaturalNumber => natVal(n.predecessor,x+1)
    }
    natVal(this,0).toString
  }
}

object Natural {
  def apply(x:Int) = {
    def sum(x:Int,n:Natural):Natural = x match {
      case 0 => n
      case _ => sum(x-1,n.successor)
    }
    sum(x,Zero)
  }
}