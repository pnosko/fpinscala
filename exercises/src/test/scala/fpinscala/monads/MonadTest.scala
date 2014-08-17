package fpinscala.monads
//import fpinscala.monoids.Monoid.{Part, Stub}

import scalaz._
import Scalaz._
import org.scalatest._
import fpinscala.monads.{Monad => M}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MonadTest extends FlatSpec with Matchers {

  it should "sequence list of lists" in {
    val l = List(1, 2)

    M.listMonad.replicateM(2, l) should be (List(List(1,2), List(1,2)))
  }
//  it should "do sth with the state" in {
//    val s = S.unit(25)
//
//    val s2 = S.set("s0")
//
//    val sg = S.get(s)
//
//    sg(20)._1 should be (25)
//  }
}
