package hb.math.numbersets

/**
  * Implicit conversions for Rational
  */
object RationalImplicits {

  import IntegerImplicits._

  implicit def rationalFromInt(i: Integer): Rational = new Rational(i)

  implicit def rationalToDouble(r: Rational): Double = {
    val n: Int = r.n
    val d: Int = r.d
    n.toDouble / d.toDouble
  }
}

/**
  * Class representing rational numbers
  */
class Rational(numerator: Integer, denominator: Integer = Integer(1)) extends Ordered[Rational] {
  require(denominator != IntegerZero)

  private def abs(x: Integer) = if (x > IntegerZero) x else -x

  private val g = gcd(abs(numerator), abs(denominator))
  val n: Integer = numerator / g
  val d: Integer = denominator / g

  private def gcd(a: Integer, b: Integer): Integer = {
    if (b == IntegerZero) a
    else gcd(b, a % b)
  }

  def reciprocal: Rational = new Rational(d, n)

  def unary_-(): Rational = new Rational(-n, d)

  def *(that: Rational): Rational = new Rational(this.n * that.n, this.d * that.d)

  def +(that: Rational): Rational = new Rational(this.n * that.d + that.n * this.d, this.d * that.d)

  def -(that: Rational): Rational = this + (-that)

  def /(that: Rational): Rational = {
    if (that.n == IntegerZero) throw new ArithmeticException("Division by zero!")
    this * that.reciprocal
  }

  override def toString: String = if (d != Integer(1)) s"$n/$d" else s"$n"

  override def compare(that: Rational): Int = IntegerImplicits.integerToInt((this - that).n)

  override def equals(o: scala.Any): Boolean = o match {
    case i: Integer => n == i && d == Integer(1)
    case Rational(rn, rd) => n == rn && d == rd
    case _ => false
  }

  /**
    * Convert a rational into an integer part and a rational part
    *
    * @return Tuple of whole part und optional rational part. The rational part is none if the rational number
    *         represents a whole number
    */
  def toInt: (Integer, Option[Rational]) = (n / d, if (n % d == IntegerZero) None else Some(Rational(n % d, d)))
}

/**
  * Companion object for Rational for easy construction
  */
object Rational {
  def apply(n: Integer, d: Integer) = new Rational(n, d)

  def unapply(arg: Rational): Option[(Integer, Integer)] = Some(arg.n, arg.d)
}
