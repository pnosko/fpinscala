package fpinscala.laziness

import Stream._
import scalaz._
import Scalaz._
//import scalaz.std.option._
import org.scalatest._

class StreamTest extends FlatSpec with Matchers {

  val stream = cons(1, cons(2, cons(3, Stream.empty[Int])))

  it should "support toList" in {
    val list = stream.toList

    list should be (List(1, 2, 3))
  }

  it should "support take" in {
    val list = stream.take(2).toList

    list should be (List(1, 2))
  }

  it should "support drop" in {
    val list = stream.drop(2).toList

    list should be (List(3))
  }

  it should "support takeWhile" in {
    val list = stream.takeWhile(_<3).toList

    list should be (List(1, 2))
  }

  it should "support forAll (true)" in {
    val list = stream.forAll(_<4)

    list should be (true)
  }

  it should "support forAll (false)" in {
    val list = stream.forAll(_<3)

    list should be (false)
  }

  it should "support headOption NE" in {
    val v = stream.headOption

    v should be (1.some)
  }

  it should "support headOption empty" in {
    val v = Stream.empty[Int].headOption

    v should be (none[Int])
  }

  it should "support map" in {
    val list = stream map {_+1} toList

    list should be (List(2, 3, 4))
  }

  it should "support append" in {
    val list = stream append 4 toList

    list should be (List(1, 2, 3, 4))
  }

  it should "support flatmap" in {
    val list = stream flatMap {x => cons(x, cons(x + 1, Stream.empty[Int]))} toList

    list should be (List(1, 2, 2, 3, 3, 4))
  }

  it should "support constant" in {
    val list = Stream.constant(3).take(4).toList

    list should be (List(3, 3, 3, 3))
  }

  it should "support from" in {
    val list = Stream.from(3).take(4).toList

    list should be (List(3, 4, 5, 6))
  }

  it should "support unfold" in {
    val list = Stream.unfold(0)(x => Some((x + 1, x + 2))).take(4).toList

    list should be (List(1, 3, 5, 7))
  }

  it should "support fibs" in {
    val list = Stream.fibs.take(5).toList

    list should be (List(0, 1, 1, 2, 3))
  }

  it should "support zipWith same lenght" in {
    val list = stream.zipWith(fibs.take(3))(_*_).toList

    list should be (List(0, 2, 3))
  }

  it should "support zipAll different lenght" in {
    val list = stream.zipAll(cons(2, cons(3, Stream.empty[Int]))).toList

    list should be (List((1.some, 2.some), (2.some, 3.some), (3.some, None)))
  }

  it should "support tails" in {
    val list = stream.tails.map(_.toList).toList

    list should be (List(List(1,2,3), List(2,3), List(3), List()))
  }

//  it should "support scan" in {
//    val list = stream.scanRight(0){_+_}.toList
//
//    list should be (List(6,5,3,0))
//  }
}