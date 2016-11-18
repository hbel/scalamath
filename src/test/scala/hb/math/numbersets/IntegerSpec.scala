package hb.math.numbersets

import IntegerImplicits._
import org.scalatest._

/**
  * Created by hendrik on 12.11.16.
  */
class IntegerSpec extends FlatSpec with Matchers {
  "An integer number" should "be displayable and convertable" in {
    val n1 = Integer(10)
    n1.toString should be("10")
    val nli: Int = n1
    nli should be(10)
    val n2 = Integer(2)
    n2.toString should be("2")
    val z1 = Zero
    z1.toString should be("0")
    val z2 = Integer(0)
    z2.toString should be("0")
    IntegerZero.toInt should be(0)
    val i: Int = Integer(25)
    i should be(25)
  }

  "Two numbers" should "be addable" in {
    val n1 = Integer(10)
    val n2 = Integer(2)
    val n3 = n1 + n2
    n3.toString should be("12")
    (n3 + IntegerZero).toString should be("12")
  }

  it should "be multiplicable" in {
    val n1 = Integer(10)
    val n2 = Integer(2)
    val n3 = n1 * n2
    n3.toString should be("20")
    (n3 * IntegerZero).toString should be("0")
  }

  it should "be subtractable" in {
    val n1 = Integer(10)
    val n2 = Integer(2)
    val n3 = n1 - n2
    n3.toString should be("8")
    (n3 - IntegerZero).toString should be("8")
    (n2 - n1).toString should be("-8")
  }

  it should "be divisable" in {
    val n1 = Integer(10)
    val n2 = Integer(2)
    (n1 / n2).toString should be("5")
    (n1 % n2).toString should be("0")
    (n2 / n1).toString should be("0")
    (n2 % n1).toString should be("2")
    (IntegerZero / n2).toString should be("0")
    a[ArithmeticException] should be thrownBy {
      n2 / IntegerZero
    }
  }

  it should "be equatable" in {
    val n1 = Integer(10)
    val n2 = Integer(2)
    (n1 > n2) should be(true)
    (n2 < n1) should be(true)
    (n2 < n2) should be(false)
    (n2 == Integer(2)) should be(true)
    (n2 == n1) should be(false)
    (n1 < n2) should be(false)
    (n2 > n1) should be(false)
  }
}
