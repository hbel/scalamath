import org.scalatest._

class PointSpec extends FlatSpec with Matchers {

  "A point" should "store x and y values as specified" in {
    val p = Point(10,-20)
	p.x should be (10)
	p.y should be (-20)
  }

  it should "be reflected by the y axis" in {
    val p = Point(10,-20)
	val p2 = p.reflectY
	p2.x should be (-10)
	p2.y should be (-20)
  }
  
  it should "be reflected by the x axis" in {
    val p = Point(10,-20)
	val p2 = p.reflectX
	p2.x should be (10)
	p2.y should be (20)
  }

  it should "flipped" in {
    val p = Point(10,-20)
	val p2 = p.flip
	p2.x should be (-10)
	p2.y should be (20)
  }

  it should "be moved by -10,20" in {
    val p = Point(10,-20)
	val p2 = p+Point(-10,20)
	p2.x should be (0)
	p2.y should be (0)
  }
}
