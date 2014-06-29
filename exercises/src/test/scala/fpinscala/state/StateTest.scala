package fpinscala.state

import scalaz._
import Scalaz._
import org.scalatest._
import fpinscala.state.RNG._

class StateTest extends FlatSpec with Matchers {

//  lazy val seed = Simple(1L)
//  val firstInt = 384748
//
//  it should "give int" in {
//    int(seed)._1 should be (firstInt)
//  }
//
//  it should "give non-negative int" in {
//    nonNegativeInt(seed)._1 should be (firstInt)
//  }
//
//  it should "support double int" in {
//    val ((integer, _), _) = intDouble(seed)
//    integer should be (firstInt)
//  }
//
//  it should "do nothing on empty machine" in {
//    val machine = Machine(true, 0, 0)
//    val input = List(Coin)
//    val output = ((0, 0), machine)
//
//    State.simulateMachine(input)(machine) should be (output)
//  }
//
//  it should "unlock on a locked machine with candy" in {
//    val machine = Machine(true, 1, 0)
//    val input = List(Coin)
//    val output = ((1, 1), Machine(false, 1, 1))
//
//    State.simulateMachine(input)(machine) should be (output)
//  }

}