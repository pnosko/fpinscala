package fpinscala.parallelism

import scalaz._
import Scalaz._
import org.scalatest._
import Par._
import java.util.concurrent.{Executors, ExecutorService}

class ParTest extends FlatSpec with Matchers {

  behavior of "Par"

//  it should "support sequence" in {
//    val list = List(unit(1), unit(2))
//    sequence(list)(Executors.newCachedThreadPool()).get should be (List(1, 2))
//  }
//
//  it should "support filter" in {
//    val list = (1 to 4).toList
//    parFilter(list)(_%2>0) should be (List(1,3))
//  }
}