package fpinscala.parsing

import fpinscala.testing._
import Gen._
import Prop._

import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = ???

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(Nil)
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(Nil)
    // map2(or(p map (List(_)), succeed(Nil)), many(p))(_ ++ _) // consecutive matches

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p)(x => succeed(f(x)))

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = map2UsingFlatmap(p, p2)(f)

  private def map2UsingFlatmap[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = for {
    a <- p
    b <- p2
  } yield f(a,b)

  private def map2Simple[C, B, A](p: Parser[A], p2: => Parser[B], f: (A, B) => C): Parser[C] = {
    map(product(p, p2))(f.tupled)
  }

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

  def count[A](p: Parser[A]): Parser[Int] = map(slice(many(p)))(_.size)

  def char(c: Char): Parser[Char] = ???

  def string(s: String): Parser[String] = ???

  def run[A](p: Parser[A])(input: String): Either[ParseError,A] = ???

  def slice[A](p: Parser[A]): Parser[String] = ???

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = for {
    a <- p
    b <- p2
  } yield ((a, b))

  // error reporting

  def label[A](msg: String)(p: Parser[A]): Parser[A] = ???

  def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???

  def attempt[A](p: Parser[A]): Parser[A] = ???

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  implicit def regex(r: Regex): Parser[String] = ???

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2:  => Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def many: Parser[List[A]] = self.many(p)
    def many1: Parser[List[A]] = self.many1(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)
    def slice: Parser[String] = self.slice(p)
    def product[B](p2: => Parser[B]) = self.product(p, p2)
    def **[B](p2: => Parser[B]) = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def listOfN(n: Int): Parser[List[A]] = self.listOfN(n, p)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def anyCharLaw[A](in: Gen[Char]): Prop =
      forAll(in)(c => run(char(c))(c.toString) == Right(c))

    def anyStringLaw[A](in: Gen[String]): Prop =
      forAll(in)(s => run(string(s))(s) == Right(s))

//    def successLaw[A](as: Gen[A])(in: Gen[String]): Prop =
//      forAll(as){a =>
//        !forAll(in)(s => run(succeed(a))(s) == Right(a)).run().isFalsified
//      }
//
//    def productLaw[A](p: Parser[A], cont: Parser[A])(in: Gen[String]): Prop =
//      forAll(in){s1 =>
//        forAll(in){s2 =>
//          val p1 = string(s1).many
//          val p2 = string(s2).many1
//
//          run(p2)(s2) == run(p1 ** p2)(s2)
//        }
//      }
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}