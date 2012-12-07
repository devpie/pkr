package de.thm.pkr

object Main extends App {
  val f = (x:Int) => (y:Int) => (z:Int) => x + y + z
  println(f(1)(2)(3))
  def f(a: Int, b: Int): Int = {
    if(a>b) {
      0
    } else {
      val h: Int => (Int, Int) => Int = 
        x => 
          if (x%2 == 0) {_ + _}
          else {_ - _}
      h(a)(0,a) + f(a+1,b)
    }
  }
  f(5,10)
}
