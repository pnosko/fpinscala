package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = fold(t)(x => 1)(1 + _ + _)

  def maximum(t: Tree[Int]) : Int = fold(t)(x => x)(_ max _)

  def depth[A](t: Tree[A]): Int = fold(t)(x => 0)((dl, dr) => (dl max dr) + 1)

  def fold[A, B](t: Tree[A])(leaf: A => B)(branch: (B, B) => B): B = t match {
    case Leaf(v) => leaf(v)
    case Branch(left, right) => branch(fold(left)(leaf)(branch), fold(right)(leaf)(branch))
  }
}