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
class Rational(private val numerator: Int, private val denominator: Int = 1) extends Ordered[Rational] {
  if (denominator == 0) throw new ArithmeticException("Denominator can't be zero!")
  val n = numerator / gcd(numerator, denominator)
  val d = denominator / gcd(numerator, denominator)

  private def gcd(a: Int, b: Int): Int = {
    import Math.abs
    val (x, y) = if (abs(a) > abs(b)) (b, a) else (a, b)
    if (x == 0) abs(y)
    else gcd(x, y % x)
  }

  def inv: Rational = new Rational(d, n)

  def *(that: Rational): Rational = new Rational(this.n * that.n, this.d * that.d)

  def +(that: Rational): Rational = new Rational(this.n * that.d + that.n * this.d, this.d * that.d)

  def -(that: Rational): Rational = this + Rational(that.n * -1, that.d)

  def /(that: Rational): Rational = {
    if (that.n == 0) throw new ArithmeticException("Division by zero!")
    this * that.inv
  }

  override def toString: String = if (d != 1) s"$n/$d" else s"$n"

  override def compare(that: Rational): Int = (this - that).n

  override def equals(o: scala.Any): Boolean = o match {
    case i: Int => n == i && d == 1
    case r: Rational => n == r.n && d == r.d
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
}
