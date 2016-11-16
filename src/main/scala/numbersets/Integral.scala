package numbersets

object IntegralImplicits {
  implicit def fromInt(i: Int): Integral = Integral(i)

  implicit def toInt(nat: Integral): Int = {
    def natVal(n: Integral, x: Int): Int = n match {
      case IntegralZero => x
      case p: PositiveNumber => natVal(p.predecessor, x + 1)
      case n: NegativeNumber => natVal(n.successor, x - 1)
    }
    natVal(nat, 0)
  }
}

/**
  * Created by hendrik on 16.11.16.
  */
trait Integral {
  def isPositive: Boolean = true

  def +(x: Integral): Integral

  def -(x: Integral): Integral

  def successor: Integral

  def predecessor: Integral

  def unary_- : Integral

  override def toString: String = IntegralImplicits.toInt(this).toString
}

/**
  * Representation of 0 as an integer number
  */
object IntegralZero extends Integral {

  def +(x: Integral) = x

  def -(x: Integral) = -x

  def successor = new PositiveNumber(this)

  def predecessor = new NegativeNumber(this)

  def unary_- = this
}

class PositiveNumber(val p: Integral) extends Integral {
  def +(x: Integral) = x match {
    case IntegralZero => this
    case p: PositiveNumber => this.successor + p.predecessor
    case n: NegativeNumber => this.predecessor + n.successor
  }

  def -(x: Integral) = x match {
    case IntegralZero => this
    case p: PositiveNumber => this.predecessor - p.predecessor
    case n: NegativeNumber => this.successor - n.successor
  }

  def successor = new PositiveNumber(this)

  def predecessor = p

  def unary_- = {
    def inv(res: Integral, cnt: Integral): Integral = cnt match {
      case IntegralZero => res
      case _ => inv(res.predecessor, cnt.predecessor)
    }
    inv(IntegralZero, this)
  }
}

class NegativeNumber(val s: Integral) extends Integral {
  def +(x: Integral) = x match {
    case IntegralZero => this
    case p: PositiveNumber => this.successor + p.predecessor
    case n: NegativeNumber => this.predecessor + n.successor
  }

  def -(x: Integral) = x match {
    case IntegralZero => this
    case p: PositiveNumber => this.predecessor - p.predecessor
    case n: NegativeNumber => this.successor - n.successor
  }

  def successor = s

  def predecessor = new NegativeNumber(this)

  def unary_- = {
    def inv(res: Integral, cnt: Integral): Integral = cnt match {
      case IntegralZero => res
      case _ => inv(res.successor, cnt.successor)
    }
    inv(IntegralZero, this)
  }
}

object Integral {
  private def sum(x: Int, n: Integral): Integral = x match {
    case 0 => n
    case _ => sum(x - 1, n.successor)
  }

  private def nsum(x: Int, n: Integral): Integral = x match {
    case 0 => n
    case _ => nsum(x + 1, n.predecessor)
  }

  def apply(x: Int): Integral = x match {
    case 0 => IntegralZero
    case i if i > 0 => sum(x, IntegralZero)
    case i if i < 0 => nsum(x, IntegralZero)
  }
}

