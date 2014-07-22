package fpinscala.monoids

import java.util.concurrent._
import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

trait FuncExtension {
  implicit class FuncOps[A, B, C](val f: A => (B => C)) {
    def flip: B => A => C = b => f(_)(b)
  }
}

object Monoid extends FuncExtension {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    def zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    def zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    def zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    def zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
    def zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A) = a1 andThen a2
    def zero = x => x
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  //trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = forAll(gen){x =>
    m.op(x, m.zero) == x && m.op(m.zero, x) == x
  } && forAll(gen.tupled3){ case (x, y, z) =>
    m.op(x, m.op(y, z)) == m.op(m.op(x, y), z)
  }

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = foldMapUsingFold(as, m)(f)

  private
  def foldMapUsingFold[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b,a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldRightUsingFoldMap(as)(z)(f)

  private
  def foldRightUsingFoldMap[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as, endoMonoid[B])(f.curried(_))(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = foldMap(as, endoMonoid[B])(f.curried.flip(_))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B  =
    if (as.isEmpty)
      m.zero
    else if (as.length == 1)
      f(as.head)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  type SortState = ((Int, Int), Boolean)
  implicit val ex: ExecutorService = Executors.newCachedThreadPool()
  // TODO: Think if this can be simplified - seems WAY too complicated
  def ordered(ints: IndexedSeq[Int]): Boolean =
    parFoldMap(ints, new Monoid[SortState] {
      def op(a1: SortState, a2: SortState): SortState =
        (a1, a2) match {
          case (((l1, u1), true), ((l2, u2), true)) => ((l1, u2), l2 >= u1)
          case (((l1, _), _), ((_, u2), _)) => ((l1, u2), false)
       }
      def zero: SortState = ((Int.MinValue, Int.MinValue), true)
    })(x => ((x,x), true)).map(_._2).run(ex)

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]): Par[A] = a1.map2(a2)(m.op(_,_))
    def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    foldMapV(v, par(m))(x => Par.unit(f(x)))

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC) = (a1, a2) match {
      case (Stub(a), Stub(b)) => Stub(a + b)
      case (Stub(a), Part(l, c, r)) => Part(a + l, c, r)
      case (Part(l, c, r), Stub(b)) => Part(l, c, r + b)
      case (Part(l1, c1, r1), Part(l2, c2, r2)) => Part(l1 + l2, c1 + c2, r1 + r2)
    }
    def zero = Stub("")
  }

  def count(s: String): Int = {
    foldMapV(s.toIndexedSeq, wcMonoid){ ch =>
      if (ch.isWhitespace)
        Part("", 1, "")
      else
        Stub(ch.toString)
    } match {
      case Stub(a) => 1
      case Part(_, c, _) => c
    }
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
      def zero: (A, B) = (A.zero, B.zero)
    }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def op(a1: A => B, a2: A => B): A => B = a => B.op(a1(a), a2(a))
      def zero: A => B = a => B.zero
    }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map()
      def op(a: Map[K, V], b: Map[K, V]) =
        a.foldLeft(b) {
          case (m,(k,v)) => m + (k -> V.op(v, m.get(k) getOrElse V.zero))
        }
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    IndexedSeqFoldable.foldMap(as)(a => Map(a -> 1))(mapMergeMonoid(intAddition))
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b,a) => mb.op(b, f(a)))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldMap(as)(List(_))(listMonoid)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
//  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
//    sys.error("todo")
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] with FuncExtension {
  import Monoid._
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    foldMap(as)(f.curried(_))(endoMonoid[B])(z)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    foldMap(as)(f.curried.flip(_))(endoMonoid[B])(z)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

//sealed trait Tree[+A]
//case class Leaf[A](value: A) extends Tree[A]
//case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

import fpinscala.datastructures.Tree

object TreeFoldable extends Foldable[Tree] with FuncExtension {
  import Monoid._
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    Tree.fold(as)(f)(mb.op)
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    foldMap(as)(f.curried.flip(_))(endoMonoid[B])(z)
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    foldMap(as)(f.curried(_))(endoMonoid[B])(z)
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as.map(f).getOrElse(mb.zero)
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    as.map(f(z, _)).getOrElse(z)
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    as.map(f(_, z)).getOrElse(z)
}

