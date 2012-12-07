package de.thm.pkr

object Main extends App {
  def compose(f: Int => Int, g: Int => Int) = {
    (x:Int) => g(f(x))
  }
  
  val f1 = List[Int=>Int](x=>2*x, x=>3*x, x=>4*x, x=>5*x)
  var F = (x:Int) => x
  
  val k = for(f <- f1) yield compose(F, f)
  
  println(F(2))
}
