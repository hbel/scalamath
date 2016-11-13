package numbersets

import org.scalatest._

/**
  * Created by hendrik on 12.11.16.
  */
class NaturalSpec extends FlatSpec with Matchers {
  "A natural number" should "be displayable and convertable" in {
    val n1 = Natural(10)
    n1.toString should be ("10")
    n1.toInt should be(10)
    val n2 = Natural(2)
    n2.toString should be ("2")
    n2.toInt should be(2)
    val z1 = Zero
    z1.toString should be ("0")
    z1.toInt should be(0)
    val z2 = Natural(0)
    z2.toString should be ("0")
    z2.toInt should be(0)
    Zero.toInt should be(0)
    Natural(25).toInt should be(25)
  }

  "Two numbers" should "be addable" in {
    val n1 = Natural(10)
    val n2 = Natural(2)
    val n3 = n1 + n2
    n3.toString should be ("12")
    (n3 + Zero).toString should be("12")
  }

  it should "be multiplicable" in {
    val n1 = Natural(10)
    val n2 = Natural(2)
    val n3 = n1 * n2
    n3.toString should be ("20")
    (n3 * Zero).toString should be("0")
  }

  it should "be subtractable" in {
    val n1 = Natural(10)
    val n2 = Natural(2)
    val n3 = n1 - n2
    n3.toString should be("8")
    (n3 - Zero).toString should be("8")
  }

  it should "be divisable" in {
    val n1 = Natural(10)
    val n2 = Natural(2)
    (n1 / n2).toString should be("5")
    (n1 % n2).toString should be("0")
    (n2 / n1).toString should be("0")
    (n2 % n1).toString should be("2")
    (Zero / n2).toString should be("0")
    a[NotANaturalNumber] should be thrownBy {
      n2 / Zero
    }
  }

  it should "result in an error to subtract a from b if a > b" in {
    a [NotANaturalNumber] should be thrownBy {
      Natural(3)-Natural(4)
    }
  }

  it should "be equatable" in {
    val n1 = Natural(10)
    val n2 = Natural(2)
    (n1>n2) should be (true)
    (n2<n1) should be (true)
    (n2<n2) should be (false)
    (n2==Natural(2)) should be (true)
    (n2==n1) should be (false)
    (n1<n2) should be (false)
    (n2>n1) should be (false)
  }
}
