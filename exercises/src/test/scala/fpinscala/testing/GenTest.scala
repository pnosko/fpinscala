package fpinscala.testing

import fpinscala.state._
import Gen._
import Prop._
import scalaz._
import org.scalatest._

class GenTest extends FlatSpec with Matchers {
  it should "support unit" in {
    val gen = Gen.unit(1)
    gen.sample(RNG.seed)._1 should be (1)
  }

  it should "support listofN" in {
    val listGen = listOfN(3, Gen.int)
    val list = listGen.sample(RNG.seed)._1

    list.toSet.size should be > 1
  }

  it should "support union" in {
    val g1 = Gen.unit(1)
    val g2 = Gen.unit(2)

    val list: List[Int] = union(g1, g2).listOfN(Gen.unit(4)).sample(RNG.seed)._1
    list.toSet.size should be > 1
  }

  it should "generate tuple of ints" in {
    val limit = 3
    val taken = Gen.unit(4)
    val g1 = Gen.choose(0, limit)

    val list = g1.listOfN(Gen.unit(2)).map(l => (l.head, l.tail.head)).listOfN(taken).sample(RNG.seed)._1
    list.toSet.size should be > 1
  }
}

