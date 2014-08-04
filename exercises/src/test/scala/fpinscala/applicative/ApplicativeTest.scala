package fpinscala.applicative

import scalaz._
import Scalaz._
import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ApplicativeTest extends FlatSpec with Matchers {

  it should "stream sequence" in {
    val a = List(Stream(1, 2, 3), Stream(1), Stream(9, 8, 7))

    val res = Applicative.streamApplicative.sequence(a)
    println(res.toList)
  }
}
