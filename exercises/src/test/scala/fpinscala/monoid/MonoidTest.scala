package fpinscala.monoids

import scalaz._
import Scalaz._
import org.scalatest._
import fpinscala.monoids.{Monoid => M}

class MonoidTest extends FlatSpec with Matchers {

  it should "support option monoid" in {
    val a1 = none[Int]
    val a2 = 3.some   // ehm :)

    M.optionMonoid.op(a1, a2) should be (3.some)
  }

  it should "support endofunction monoid" in {
    val a1: Int => Int = _ * 3
    val a2: Int => Int = _ - 2

    val f = M.endoMonoid.op(a1, a2)
    f(1) should be (1)
  }

  it should "support foldRight using foldMap" in {
    val as = List(1, 2, 3)
    val m = M.intAddition
    val sum = M.foldRight(as)(m.zero)(m.op)
    sum should be (6)
  }

  it should "support foldLeft using foldMap" in {
    val as = List(1, 2, 3)
    val m = M.intAddition
    val sum = M.foldLeft(as)(m.zero)(m.op)
    sum should be (6)
  }

  import fpinscala.testing._
  import fpinscala.state.RNG
  val whatever: Int = 30
  it should "pass monoid laws for integers" in {
    val gen = Gen.choose(1, 1000)
    val m = M.intAddition
    M.monoidLaws(m, gen).run(whatever, whatever, RNG.seed).isFalsified should be (false)
  }


}