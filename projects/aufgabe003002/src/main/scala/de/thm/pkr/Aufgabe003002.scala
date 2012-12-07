package de.thm.pkr

class Basis {
  def f(x:Int) = {println(x)}
}
class Abgeleitet extends Basis {
  def f(x:Int, y:Int) = {println(x,y)}
}

object Main extends App {
  val a = new Abgeleitet()
  a.f(10)
  a.f(10,11)
}
