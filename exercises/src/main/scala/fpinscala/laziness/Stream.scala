package fpinscala.laziness

import Stream._
import scalaz._
import Scalaz._
//import syntax.std.option._


trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def toList: List[A] = foldRight(List[A]()){_::_}

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def find(f: A => Boolean): Option[A] = findRec(f)

  @annotation.tailrec
  private final def findRec(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().findRec(f)
  }

  private def findFold(f:  A => Boolean): Option[A] = filter(f).headOption

  def take(n: Int): Stream[A] =
    if (n > 0)
      this match {
        case Empty => Empty
        case Cons(h, t) => cons(h(), t().take(n - 1))
      }
    else Empty

  def drop(n: Int): Stream[A] =
    if (n <= 0)
      this
    else this match {
      case Empty => Empty
      case Cons(h, t) => t().drop(n - 1)
    }

  def takeWhile(p: A => Boolean): Stream[A] = takeWhileTrivial(p)

  private def takeWhileTrivial(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  private def takeWhileFold(p: A => Boolean): Stream[A] = foldRight(empty[A])((el, acc) =>
    if(p(el)) cons(el, acc) else empty[A]
  )

  def forAll(p: A => Boolean): Boolean = forAllTrivial(p)

  private def forAllTrivial(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if (p(h())) => t().forAll(p)
    case Empty => true
    case _ => false
  }

  def headOption: Option[A] = headOptionFold

  def headOptionFold: Option[A] = foldRight(none[A])((el, _) => Some(el))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[AA >: A](el: AA): Stream[AA] = foldRight(cons(el, empty)){cons(_, _)}

  def concat[AA >: A](other: => Stream[AA]): Stream[AA] = foldRight(other)((h,t) => cons(h, t))

  def map[B](f: A => B): Stream[B] = mapFoldRight(f)

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h) concat t)

  private def mapFoldRight[B](f: A => B): Stream[B] = foldRight(empty[B])((el, acc) => cons(f(el), acc))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = zipAllUnfold(s2)

  def zipWith[B, C](other: Stream[B])(f: (A,B) => C): Stream[C] = zipWithUnfold(other)(f)

  private def zipWithUnfold[B, C](other: Stream[B])(f: (A,B) => C): Stream[C] =
    Stream.unfold((this, other)) {
      case (Cons(ha, ta), Cons(hb, tb)) =>
        (f(ha(), hb()), (ta(), tb())).some
    }

  private def zipAllUnfold[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(ha, ta), Cons(hb, tb)) => ((ha().some, hb().some), (ta(), tb())).some
    }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h, h2) => h == h2
    }

  def tails: Stream[Stream[A]] = tailsDirect

  private def tailsDirect: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } concat (Stream(empty))

  //private def mapUnfold[B](f: A => B): Stream[B] = Stream.unfold(this)(x => x.uncons.map(s => (f(s.head), s.tail)))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = onesUnfold

  def constant[A](a: A): Stream[A] = constantTrivial(a)

  def from(n: Int): Stream[Int] = fromUnfold(n)

  def fibs: Stream[Int] = fibsUnfold

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) map {
    case (n, s) => cons(n, unfold(s)(f))
  } getOrElse empty

  private def fibsUnfold: Stream[Int] = unfold((0, 1)){case (a, b) => Some(a,(b, a + b))}

  private def fromUnfold(n: Int): Stream[Int] = unfold(n)(x => (x, x+1).some)

  private def fromTrivial(n: Int): Stream[Int] = cons(n, from(n + 1))

  private val onesTrivial: Stream[Int] = cons(1, onesTrivial)

  private val onesUnfold: Stream[Int] = constantUnfold(1)

  private def constantTrivial[A](a: A): Stream[A] =  cons(a, constantTrivial(a))

  private def constantUnfold[A](a: A): Stream[A] = unfold(a)(x => (a, a).some)
}