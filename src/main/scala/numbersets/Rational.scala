package numbersets

import scala.Option

/**
  * Implicit conversions for Rational
  */
object RationalImplicits {
  implicit def fromInt(i: Int): Rational = new Rational(i, 1)

  implicit def toDouble(r: Rational): Double = r.n.toDouble / r.d.toDouble
}

/**
  * Class representing rational numbers
  */
class Rational(numerator: Int, denominator: Int = 1) extends Ordered[Rational] {
  require(denominator != 0)

  import Math.abs

  private val g = gcd(abs(numerator), abs(denominator))
  val n = numerator / g
  val d = denominator / g

  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a
    else gcd(b, a % b)
  }

  def inv: Rational = new Rational(d, n)

  def unary_-(): Rational = new Rational(-n, d)

  def *(that: Rational): Rational = new Rational(this.n * that.n, this.d * that.d)

  def +(that: Rational): Rational = new Rational(this.n * that.d + that.n * this.d, this.d * that.d)

  def -(that: Rational): Rational = this + (-that)

  def /(that: Rational): Rational = {
    if (that.n == 0) throw new ArithmeticException("Division by zero!")
    this * that.inv
  }

  override def toString: String = if (d != 1) s"$n/$d" else s"$n"

  override def compare(that: Rational): Int = (this - that).n

  override def equals(o: scala.Any): Boolean = o match {
    case i: Int => n == i && d == 1
    case Rational(rn, rd) => n == rn && d == rd
    case _ => false
  }

  /**
    * Convert a rational into an integer part and a rational part
    *
    * @return Tuple of whole part und optional rational part. The rational part is none if the rational number
    *         represents a whole number
    */
  def toInt: (Int, Option[Rational]) = (n / d, if (n % d == 0) None else Some(Rational(n % d, d)))
}

/**
  * Companion object for Rational for easy construction
  */
object Rational {
  def apply(n: Int, d: Int) = new Rational(n, d)

  def unapply(arg: Rational): Option[(Int, Int)] = Some(arg.n, arg.d)
}
