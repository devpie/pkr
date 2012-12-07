package de.thm.pkr

object Main extends App {

  def compose(f: Int => Int, g: Int => Int) = {
    (x:Int) => g(f(x))
  }
  
  val f1 = List[Int](1,2,3,4,5)
  
  var F = (x:Int) => x
  
  var i = -1
  
  while(i<f1.length-1) {
    i += 1
    val f = ((a: Int) => (x:Int) => f1(a)*x)(i)
    F = compose(F, f)
  }
  
  println(F(2))
}
