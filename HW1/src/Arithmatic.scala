package arithmetic

// Arithmetic Expressions
// EXPRESSION ::= NUMERAL | ( EXPRESSION OPERATOR EXPRESSION )
// OPERATOR ::= + | -
// NUMERAL is a sequence of digits from the set, {0, 1, 2, ..., 9}
import Arithmetic.wholeNumber

import scala.util.parsing.combinator.JavaTokenParsers

trait ArithOpTree {
  // Operator Tree (Abstract Syntax Tree)
  sealed abstract class Etree
  case class Add(e1: Etree, e2: Etree) extends Etree
  case class Sub(e1: Etree, e2: Etree) extends Etree
  case class Mul(e1: Etree, e2: Etree) extends Etree
  case class Div(e1: Etree, e2: Etree) extends Etree
  case class Number(s: String) extends Etree
}

object Arithmetic extends JavaTokenParsers with ArithOpTree{
  // Parser
  def parse(source: String): Etree =
    parseAll(expr, source) match {
      case Success(optree,_) => optree
      case _ => throw new Exception("Parse error!")
    }

  def expr: Parser[Etree] =  term ~ rep("[+-]".r ~ term) ^^ {
    case t ~ ts => ts.foldLeft(t) {
      case (e1, "+" ~ e2) => Add(e1, e2)
      case (e1, "-" ~ e2) => Sub(e1, e2)
    }
  }

  def term = factor ~ rep("[*/]".r ~ factor) ^^ {
    case e ~ es => es.foldLeft(e) {
      case (e1, "*" ~ e2) => Mul(e1, e2)
      case (e1, "/" ~ e2) => Div(e1, e2)
    }
  }

  def factor = "(" ~> expr <~ ")" | wholeNumber ^^ (Number(_))

  def op: Parser[String] = "+" | "-" | "*" | "/"
  // Interpreter
  def eval(t: Etree): Int = t match {
    case Number(s) => s.toInt
    case Add(e1,e2) => eval(e1) + eval(e2)
    case Sub(e1,e2) => eval(e1) - eval(e2)
    case Mul(e1,e2) => eval(e1) * eval(e2)
    case Div(e1,e2) => eval(e1) / eval(e2)
  }
  // Controller
  def main(args: Array[String]): Unit = {
    try {
      val source = args(0)
      println("input : " + source)
      val optree = parse(source)
      println("optree : " + optree)
      println("result : " + eval(optree))
    }
    catch { case e: Exception => println(e)}
  }
}