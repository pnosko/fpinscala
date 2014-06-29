package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = sumSimple(ints)

  private def sumSimple(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sumSimple(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  private def sumFold(ints: List[Int]): Int = foldLeft(ints, 0){_+_}

  def product(ds: List[Double]): Double = productFold(ds)

  private def productSimple(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * productSimple(xs)
  }

  private def productFold(ds: List[Double]): Double = foldLeft(ds, 1.0){_*_}

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldRightUsingFoldLeft(as, z)(f)

  private def foldRightUsingFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  private def foldRightRecursive[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRightRecursive(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception("no tail")
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, tail(l))

  def drop[A](l: List[A], n: Int): List[A] = if (n == 0) l else drop(tail(l), n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
    case Nil => Nil
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldLeft(l, 0)((len, _) => len + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, List[B]())((x, acc) => Cons(f(x), acc))

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, elem) => Cons(elem, acc))

  def flatten[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())(append)

  def increment(l: List[Int]): List[Int] = map(l){_+1}

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = flatten(map(l)(f))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l){x => if(f(x)) List(x) else Nil }

  def zip[A, B](l: List[A], r: List[B]): List[(A, B)] = zipWith(l, r){(t1, t2) => (t1, t2)}

  def zipWith[A,B,C](l: List[A], r: List[B])(f: (A,B) => C): List[C] =
    (l, r) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(lx, lxs), Cons(rx, rxs)) => Cons(f(lx, rx), zipWith(lxs, rxs)(f))
    }

  def addPairs(a: List[Int], b: List[Int]): List[Int] = zipWith(a, b) {_+_}
}