package fpinscala.applicative

import fpinscala.applicative.{Applicative => A}
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

    A.validationApplicative.map2(a, b)(List(_, _)).errors.toList should be (List("a head", "a tail", "b head", "b tail"))
  }

  it should "sequence over a map" in {
    val m = Map("a" -> 2.some, "b" -> 3.some)

    val ap: Applicative[({type M[F[_]] = Map[String, F[_]]})#M] = new Applicative[({type M[F[x]] = Map[String, F[_]]})#M] {
      override def unit[A](a: => A): Map[String, A] = Map[String, A]()
      override def map2[A,B,C](fa: Map[String, A], fb: Map[String, B])(f: (A, B) => C): Map[String,C] = ???
    }

    ap.sequenceMap(m)
  }
}
