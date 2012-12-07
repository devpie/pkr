package de.thm.pkr

class Node[T](initVal: T, left: Option[Node[T]], right: Option[Node[T]]) {
  override def toString = "[" + initVal + "," + left + "," + right + "]"
  def map(f: T => T): Node[T] = {
    Node(f(initVal), left map { _ map f }, right map { _ map f })
  }
  def foreach[Unit](f: Node[T] => Unit) {
    f(this)
    left foreach { _ foreach f }
    right foreach { _ foreach f }
  }
}

object Node {
  def apply[T](initVal: T)
  	= apply[T](initVal, None, None)
  def apply[T](initVal: T, left: Node[T])
  	= apply[T](initVal: T, Some(left), None)
  def apply[T](initVal: T, left: Node[T], right: Node[T])
  	= apply[T](initVal, Some(left), Some(right))
  def apply[T](initVal: T, left: Option[Node[T]], right: Node[T])
  	= apply[T](initVal, left, Some(right))
  def apply[T](initVal: T, left: Option[Node[T]], right: Option[Node[T]])
    = new Node[T](initVal, left, right)
}

object Main extends App {
  val tree = Node(1, Node(2,Node(4)), Node(3))
  println(tree)
  println(tree map { _ * 10 })
  val tree2 = Node("J",Node("a"), Node("n"))
  println(tree2)
  println(tree2 map { _ toUpperCase })
}

