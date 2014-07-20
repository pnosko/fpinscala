package fpinscala.parsing

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}

object Parsing {
  type Parser[+A] = String => Either[ParseError,A]
}

//class Parser[+A]() {
//
//}
//
//trait thingy {
//  type Parser[+A] = String => Either[ParseError, A]
//
//  object ConcreteParsers extends Parsers[Parser] {
//    def string(s: String): Parser[A] =
//      (input: String) =>
//        if (input.startsWith(s))
//          Right(s)
//        else
//          Left(Location(input).toError("Expected: " + s))
//  }
//}

