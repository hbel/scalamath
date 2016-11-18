package hb.math.numbersets

import org.scalatest._
import IntegerImplicits._
import RationalImplicits._

/**
  * Created by hendrik on 15.11.16.
  */
class RationalSpec extends FlatSpec with Matchers {
  "A rational number" should "be displayable and convertable" in {
    Rational(1, 2).toString should be("1/2")
    val d2: Double = RationalImplicits.rationalToDouble(Rational(1, 2))
    d2 should be(0.5)
    Rational(2, 4).toString should be("1/2")
    val d3: Double = RationalImplicits.rationalToDouble(Rational(2, 4))
    d3 should be(0.5)
    Rational(17, 31).toString should be("17/31")
    val d4: Double = RationalImplicits.rationalToDouble(Rational(17, 31))
    d4 should be(0.5483870967741935)
    Rational(22, 11).toString should be("2")
  }

  it should "be multiplyable with Rationals and Ints" in {
    (Rational(1, 2) * Integer(2)).toString should be("1")
    (Rational(1, 2) * Rational(2, 3)).toString should be("1/3")
  }

  it should "be divisable with Rationals and Ints" in {
    (Rational(1, 2) / Integer(2)).toString should be("1/4")
    (Rational(1, 2) / Rational(2, 3)).toString should be("3/4")
  }

  it should "provide the inverted rational b/a" in {
    Rational(1, 2).reciprocal.toString should be("2")
    Rational(3, 4).reciprocal.toString should be("4/3")
  }

  it should "be negatable" in {
    val n1 = Rational(1, 2)
    val n2 = -n1
    n2.toString should be("-1/2")
    val n3 = -n2
    n3.toString should be("1/2")
  }

  it should "be addable with Rationals and Ints" in {
    (Rational(1, 2) + Integer(2)).toString should be("5/2")
    (Rational(1, 2) + Rational(2, 3)).toString should be("7/6")
  }

  it should "be subtractable with Rationals and Ints" in {
    (Rational(1, 2) - Integer(2)).toString should be("-3/2")
    (Rational(1, 2) - Rational(2, 3)).toString should be("-1/6")
  }

  it should "be comparable with Rationals and Ints" in {
    (Rational(1, 2) < 2) should be(true)
    (Rational(1, 2) < Rational(2, 3)) should be(true)
    (Rational(1, 2) > 2) should be(false)
    (Rational(1, 2) > Rational(2, 3)) should be(false)
    (Rational(4, 2) == Integer(2)) should be(true)
    (Rational(2, 3) == Rational(2, 3)) should be(true)
  }

  it should "throw an exception on a division by zero" in {
    a[IllegalArgumentException] should be thrownBy {
      Rational(4, 0)
    }
    a[ArithmeticException] should be thrownBy {
      Rational(4, 1) / Integer(0)
    }
  }

  it should "be representable by a mix of Int and Rational" in {
    Rational(3, 2).toInt should be(Integer(1), Some(Rational(1, 2)))
    Rational(4, 2).toInt should be(Integer(2), None)
  }
}
