
package fpinscala.datastructures

import org.scalatest._
import Tree._

class TreeTest extends FlatSpec with Matchers {
	val t1: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

	it should "support depth" in {
		depth(t1) should be (2)		
	}

	it should "support maximum" in {
		maximum(t1) should be (3)		
	}

	it should "support size" in {
		Tree.size(t1) should be (5)		
	}
}