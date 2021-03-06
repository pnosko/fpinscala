package fpinscala
package applicative

import monads.Functor
import fpinscala.state._
import fpinscala.common.Util._
import StateUtil._ // defined at bottom of this file
import monoids._

trait Applicative[F[_]] extends Functor[F] { self =>

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = map2Apply(fa, fb)(f)

  private def map2Apply[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(apply(unit(f.curried))(fa))(fb)

  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B,C) => D): F[D] = map3Apply(fa, fb, fc)(f)

  private def map3Apply[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B,C) => D): F[D] = apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = applyWithMap2(fab)(fa)

  private def applyWithMap2[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_(_))

  def unit[A](a: => A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(id)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldLeft(unit(List[B]()))((lst, a) => map2(f(a),lst){_ :: _})

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def factor[A,B](fa: F[A], fb: F[A]): F[(A,B)] = ???

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = new Applicative[({type f[x] = (F[x], G[x])})#f] {
    def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
    override def map2[A,B,C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = fa.bimap(self.map2(_,fb._1)(f), G.map2(_,fb._2)(f))
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = new Applicative[({type f[x] = F[G[x]]})#f] {
    def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
    override def map2[A,B,C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] = self.map2(fa, fb)(G.map2(_,_)(f))
  }

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = ofa.foldLeft(unit(Map[K,V]())) { case (fm,(k, fv)) =>
    self.map2(fm, fv)((m, v) => m + (k -> v))
  }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] { self =>
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A,B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma.right.flatMap(f)
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
    Monad[({type f[x] = F[N[x]]})#f] = ???
}

sealed trait Validation[+E, +A] {
  def errors: Vector[E] = this match {
    case Success(a) => Vector.empty[E]
    case Failure(h, t) => h +: t
  }
}

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

//  def apply[F[_]: Functor, X](ftor: F[X], u: X => F[X])(implicit ) = new Applicative[F] {
//    override def unit[A](a: => A): F[A] = u(a)
//    override def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = ???
//  }

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] =
    new Applicative[({type f[x] = Validation[E,x]})#f] {
      def unit[A](a: => A): Validation[E, A] = Success(a)
      override def map2[A,B,C](a: Validation[E, A], b: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (a, b) match {
        case (Success(aa), Success(bb)) => Success(f(aa, bb))
        case (Success(_), bb@Failure(_, _)) => bb
        case (aa@Failure(_, _), Success(_)) => aa
        case (Failure(ah, at), Failure(bh, bt)) => Failure(ah,  at ++ (bh +: bt))
      }
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
  }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  type Id[A] = A
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A,B](a: A)(f: A => B): B = f(a)
  }

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s)
      _  <- set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =  ??? //mapAccum(fa, unit[A])((a, s) => ((), a :: s))._2

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B = mapAccum(fa, z)((a, b) => ((), f(b, a)))._2   // TODO: test if shold be reversed

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a),g(a)))(G.product(H))    // TODO: see how the passing of the Applicative instance works (what's the return type of traverse here??)

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = ???
}

object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[G[_],A,B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] = fa.foldLeft(G.unit(List[B]()))((b, a) => G.map2(f(a),b)(_::_))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_],A,B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] = oa.fold(G.unit(none[B]))((a:A) => G.map(f(a))(Some(_)))
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_],A,B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] = ???
  }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
