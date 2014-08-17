package fpinscala.common

object Util {
  def id[A](a: A): A = a

  implicit class BifunctorTuple[+A,+B](value: (A,B)) {
    def bimap[C,D](f: A => C, g: B => D): (C,D) = value match { case (aa, bb) => (f(aa), g(bb))}
  }

}
