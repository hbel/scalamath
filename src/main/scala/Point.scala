class Point(val x:Int,val y:Int){
  def +(p:Point) = Point(x+p.x,y+p.y)
  def reflectY = Point(-x,y)
  def reflectX = Point(x,-y)
  def flip = Point(-x,-y)
}

object Point{
  def apply(x:Int,y:Int) = new Point(x,y)
}
