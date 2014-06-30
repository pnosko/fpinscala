
package fpinscala.datastructures

import org.scalatest._
import List._
import org.scalatest.junit.JUnitRunner

//@RunWith(classOf[JUnitRunner])
class ListTest extends FlatSpec with Matchers {
	it should "be able to map" in {
		val list = List(1, 2, 3, 4, 5)

		map(list) {_+1} should be (List(2, 3, 4, 5, 6))
	}

	it should "support drop on nonempty" in {
		val list = List(1)
		drop(list, 1) should be (Nil)
	}

	it should "support dropWhile on nonempty" in {
		val list = List(1, 2, 3)

		dropWhile(list, (x: Int) => x < 3) should be (List(3))
	}

	it should "support product" in {
		val list = List(1.0, 2.0, 3.0)

		product(list) should be (6.0)
	}

	it should "support sum" in {
		val list = List(1, 2, 3)

		sum(list) should be (6)
	}

    it should "support reverse" in {
		val list = List(1, 2, 3)

		reverse(list) should be (List(3, 2, 1))
	}

	it should "support flatten" in {
		val list = List(List(1, 2), List(3, 4))

		flatten(list) should be (List(1, 2, 3, 4))
	}

	it should "support increment" in {
		val list = List(1, 2)

		increment(list) should be (List(2, 3))
	}

	it should "support filter" in {
		val list = List(1, 2, 3)

		filter(list)(_!=2) should be (List(1, 3))
	}

	it should "support flatMap" in {
		val list = List(1, 2, 3)

		flatMap(list)(i => List(i, i)) should be (List(1, 1, 2, 2, 3, 3))
	}

	it should "support addPairs" in {
		val list = List(1, 2, 3)
		val other = List(3, 2, 1)

		addPairs(list, other) should be (List(4, 4, 4))
	}

	// it should "support hasSubsequence" in {
	// 	val list = List(1, 2, 3, 4, 5, 5, 6)
	// 	val sub = List(4, 5, 5)

	// 	hasSubsequence(list, sub) should be (true)
	// }
}
