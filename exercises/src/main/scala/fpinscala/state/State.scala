package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  lazy val seed: RNG = Simple(1L)

  def nonNegativeInt: Rand[Int] = int map { i => if (i<0) -(i+1) else i }

  def double: Rand[Double] = int map { _ / (Int.MaxValue + 1).toDouble }

  def boolean: Rand[Boolean] = int map (_%2==0)

  def isEven(n: Int): Boolean = n % 2 == 0

  val int: Rand[Int] = _.nextInt

  val even: Rand[Int] = int map(x => if (isEven(x)) x else x + 1)

  val odd: Rand[Int] = int map(x => if (!isEven(x)) x else x + 1)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def lazyUnit[A](a: => A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = s map f

  def positiveInt: Rand[Int] = nonNegativeInt map { Math.abs(_) }

  def intDouble: Rand[(Int,Double)] = int.map2(double)((_,_))

  def doubleInt: Rand[(Double,Int)] = double.map2(int)((_,_))

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ra.map2(rb)(f)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit[List[A]](Nil))((elem, acc) => elem.map2(acc){_ :: _})

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = s => f(s) match { case (a, ss) => g(a).run(ss)}

  implicit def convert2properRand[A](r: Rand[A]): State.Rand[A] = new State(r)

  implicit def convert2dumbassRand[A](r: State.Rand[A]): Rand[A] = r.run
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State(s => run(s) match { case (a, ss) => (f(a), ss)})

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = for {
    a <- this
    b <- sb
  } yield f(a,b)

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => run(s) match { case (a, ss) => f(a).run(ss)} )

  def apply(s: S): (A, S) = run(s)
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State((a, _))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs.foldRight(unit[S, List[A]](Nil))((elem, acc) => elem.map2(acc){_ :: _})

//  def get[S]: State[S, S] = State(s => (s, s))

//  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def next(m: Machine, in: Input): Machine = (m, in) match {
      case (Machine(_, 0, _), _) => m
      case (Machine(true, cdy, coin), Coin) => Machine(false, cdy, coin + 1)
      case (Machine(false, cdy, coin), Turn) => Machine(true, cdy - 1, coin)
      case _ => m
    }

    State { m =>
      val machine = inputs.foldLeft(m)((mach, in) => next(mach, in))
      ((machine.candies, machine.coins), machine)
    }
  }
}