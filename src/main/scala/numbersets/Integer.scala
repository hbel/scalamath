package numbersets

/**
  * Implicit conversions between Integral and Int
  */
object IntegerImplicits {
  implicit def integerFromInt(i: Int): Integer = Integer(i)

  implicit def integerToInt(nat: Integer): Int = {
    def natVal(n: Integer, x: Int = 0): Int = n match {
      case IntegerZero => x
      case p: PositiveNumber => natVal(p.predecessor, x + 1)
      case n: NegativeNumber => natVal(n.successor, x - 1)
    }
    natVal(nat)
  }
}

/**
  * Integer numbers
  */
trait Integer extends Ordered[Integer] {
  def +(x: Integer): Integer

  def -(x: Integer): Integer

  def *(x: Integer): Integer

  def /(x: Integer): Integer

  def %(x: Integer): Integer

  def successor: Integer

  def predecessor: Integer

  def unary_- : Integer

  override def toString: String = IntegerImplicits.integerToInt(this).toString

  override def compare(that: Integer): Int = this - that match {
    case IntegerZero => 0
    case p: PositiveNumber => 1
    case _ => -1
  }
}

/**
  * Representation of 0 as an integer number
  */
object IntegerZero extends Integer {
  def +(x: Integer) = x

  def -(x: Integer) = -x

  def *(x: Integer) = IntegerZero

  def /(x: Integer) = IntegerZero

  def %(x: Integer) = IntegerZero

  def successor = new PositiveNumber(this)
  def predecessor = new NegativeNumber(this)
  def unary_- = this
}

/**
  * Trait for non-zero integers
  */
trait IntegerNumber extends Integer {
  protected def selector(i: Integer): Integer

  def +(x: Integer) = x match {
    case IntegerZero => this
    case p: PositiveNumber => this.successor + p.predecessor
    case n: NegativeNumber => this.predecessor + n.successor
  }

  def -(x: Integer) = x match {
    case IntegerZero => this
    case p: PositiveNumber => this.predecessor - p.predecessor
    case n: NegativeNumber => this.successor - n.successor
  }

  def unary_- = {
    def inv(res: Integer, cnt: Integer): Integer = cnt match {
      case IntegerZero => res
      case _ => inv(selector(res), selector(cnt))
    }
    inv(IntegerZero, this)
  }

  protected def product(n: Integer, acc: Integer = IntegerZero): Integer = n match {
    case IntegerZero => acc
    case n: Integer => product(n.predecessor, this + acc)
  }

  protected def div(x: Integer, remainder: Integer, acc: Integer = IntegerZero): Integer = {
    if (remainder < x) acc
    else div(x, remainder - x, acc.successor)
  }

  protected def mod(x: Integer, acc: Integer): Integer = if (acc < x) acc else mod(x, acc - x)

  override def equals(o: scala.Any): Boolean = o match {
    case i: IntegerNumber => this - i == IntegerZero
    case _ => false
  }
}

/**
  * Positive integer
  *
  * @param p predecessor integer
  */
class PositiveNumber(p: Integer) extends IntegerNumber {
  def successor = new PositiveNumber(this)

  def predecessor = p

  def selector(i: Integer) = i.predecessor

  def *(x: Integer) = x match {
    case p: PositiveNumber => product(x)
    case n: NegativeNumber => -product(-x)
    case _ => IntegerZero
  }

  def /(x: Integer) = x match {
    case p: PositiveNumber => div(x, this)
    case n: NegativeNumber => -div(-x, this)
    case _ => throw new ArithmeticException("Division by zero!")
  }

  def %(x: Integer) = x match {
    case p: PositiveNumber => mod(x, this)
    case n: NegativeNumber => -mod(-x, this)
    case _ => throw new ArithmeticException("Division by zero!")
  }
}

/**
  * Negative integer
  *
  * @param s successor integer
  */
class NegativeNumber(s: Integer) extends IntegerNumber {
  def successor = s

  def predecessor = new NegativeNumber(this)

  def selector(i: Integer) = i.successor

  def *(x: Integer) = x match {
    case p: PositiveNumber => -(-this.product(x))
    case n: NegativeNumber => -this.product(-x)
    case _ => IntegerZero
  }

  def /(x: Integer) = x match {
    case p: PositiveNumber => -div(x, -this)
    case n: NegativeNumber => div(-x, -this)
    case _ => throw new ArithmeticException("Division by zero!")
  }

  def %(x: Integer) = x match {
    case p: PositiveNumber => -mod(x, -this)
    case n: NegativeNumber => mod(-x, -this)
    case _ => throw new ArithmeticException("Division by zero!")
  }
}

/**
  * Companion object containing the apply function to construct Integers from Ints
  */
object Integer {
  def apply(x: Int): Integer = {
    def sum(x: Int, acc: Int => Int, nextNumber: Integer => Integer): Integer = {
      def s(x: Int, n: Integer): Integer = x match {
        case 0 => n
        case _ => s(acc(x), nextNumber(n))
      }
      s(x, IntegerZero)
    }
    x match {
      case 0 => IntegerZero
      case i if i > 0 => sum(x, _ - 1, _.successor)
      case i if i < 0 => sum(x, _ + 1, _.predecessor)
    }
  }
}

