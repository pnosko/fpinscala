package fpinscala.monoids

import fpinscala.monoids.Monoid.{Part, Stub}

import scalaz._
import Scalaz._
import org.scalatest._
import fpinscala.monoids.{Monoid => M}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
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

  it should "fail ordered on unordered list" in {
    val as = List(1, 2, 5, 3).toIndexedSeq
    M.ordered(as) should be (false)
  }

  it should "succeed ordered on ordered list" in {
    val as = List(1, 2, 3, 4).toIndexedSeq
    M.ordered(as) should be (true)
  }

  it should "count words" in {
    M.count("blah blah coem se faohdf ") should be (5)
  }

  it should "count words2" in {
    M.count(" blah blah a adf asdf efae  coem se faohdf ") should be (9)
  }

  it should "count words3" in {
    M.count("blah blahasdfasdffa;ohdf ") should be (2)
  }

  it should "count words4" in {
    M.count("blah blahasdfasdffa;ohdf") should be (2)
  }

  import fpinscala.testing._
  import fpinscala.state.RNG
  val whatever: Int = 30
  it should "pass monoid laws for integers" in {
    val gen = Gen.choose(1, 1000)
    val m = M.intAddition
    M.monoidLaws(m, gen).run(whatever, whatever, RNG.seed).isFalsified should be (false)
  }

  val wordGen = Gen.lcString(3, 5)
  val wordOrEmptyGen = Gen.weighted((wordGen, 0.7), (Gen.unit(" "), 0.3))
  it should "pass monoid laws for WC" in {
    val stubGen = wordGen.map(Stub(_))
    val partGen = for {
      l <- wordOrEmptyGen
      c <- Gen.choose(1, 3)
      r <- wordOrEmptyGen
    } yield (Part(l, c, r))

    val gen = Gen.boolean.flatMap(if (_) stubGen else partGen)
    val m = M.wcMonoid
    M.monoidLaws(m, gen).run(whatever, whatever, RNG.seed).isFalsified should be (false)
  }

  it should "pass monoid laws for sth" in {
    val gen = Gen.stringN(10).tupled(Gen.choose(1, 50))
    val m = M.productMonoid(M.stringMonoid, M.intAddition)
    M.monoidLaws(m, gen).run(whatever, whatever, RNG.seed).isFalsified should be (false)
  }
}