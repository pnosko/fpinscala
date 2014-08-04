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

  it should "aggregate validation errors" in {
    val a = Failure("a head", Vector("a tail"))
    val b = Failure("b head", Vector("b tail"))

    Applicative.validationApplicative.map2(a, b)(List(_, _)).errors.toList should be (List("a head", "a tail", "b head", "b tail"))
  }
}
