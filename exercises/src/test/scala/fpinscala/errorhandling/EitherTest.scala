
package fpinscala.errorhandling

import org.scalatest._
import Either._

class EitherTest extends FlatSpec with Matchers {
	it should "support traverse" in {
		val list = List(1, 2, 3)
		Either.traverse(list)(a => Right(a.toString)) should be (Right(List("1", "2", "3")))        
	}
}