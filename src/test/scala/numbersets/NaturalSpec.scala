package numbersets

import org.scalatest._

/**
  * Created by hendrik on 12.11.16.
  */
class NaturalSpec extends FlatSpec with Matchers {
  "A natural number" should "be displayable" in {
    val n1 = Natural(10)
    n1.toString should be ("10")
    val n2 = Natural(2)
    n2.toString should be ("2")
    val z1 = Zero
    z1.toString should be ("0")
    val z2 = Natural(0)
    z2.toString should be ("0")
  }
  "Two numbers" should "be addable" in {
    val n1 = Natural(10)
    val n2 = Natural(2)
    val n3 = n1 + n2
    n3.toString should be ("12")
  }

  it should "be multiplicable" in {
    val n1 = Natural(10)
    val n2 = Natural(2)
    val n3 = n1 * n2
    n3.toString should be ("20")
  }

  it should "be subtractable" in {
    val n1 = Natural(10)
    val n2 = Natural(2)
    val n3 = n1 - n2
    n3.toString should be("8")
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
