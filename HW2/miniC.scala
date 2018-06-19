// Interpreter for a C-like mini-language with pointers
//
// Program P ::= CL
// CommandList CL ::= C | C ; CL
// Command C ::= L = E | print L | while E : CL end
// Expression E ::= N | ( E1 + E2 ) | L | &L
// LefthandSide L ::= I | *L
// Numeral N ::= string of digits
// Variable I ::= strings of letters, not including keywords: while, print, end
//
// Operator Tree
// PTREE ::= List[CTREE]
// CTREE ::= Assign(LTREE,ETREE) | While(ETREE,CLIST) | Print(LTREE)
// ETREE ::= Num(String) | Add(ETREE,ETREE) | Sub(ETREE,ETREE) | At(LTREE) | Amph(LTREE)
// LTREE ::= Var(String) | Star(LTREE)

trait OpTree {
  sealed abstract class Ltree
  case class Var(x: String) extends Ltree
  case class Star(l: Ltree) extends Ltree
  case class GetArr(l:String, exp: Etree) extends Ltree

  sealed abstract class Etree
  case class Num(s: String) extends Etree
  case class Add(e1: Etree, e2: Etree) extends Etree
  case class Sub(e1: Etree, e2: Etree) extends Etree
  case class At(l: Ltree) extends Etree
  case class Amph(l: Ltree) extends Etree

  sealed abstract class Ctree
  case class Assign(l: Ltree, e: Etree) extends Ctree
  case class While(e: Etree, c: List[Ctree]) extends Ctree
  case class Print(L: Ltree) extends Ctree
  case class Decl(x: String) extends Ctree
  case class Arr(n: Int, l : String) extends Ctree
}
import scala.util.parsing.combinator.JavaTokenParsers

object MiniC extends JavaTokenParsers with OpTree {
  // Parser
  def parse(source: String): List[Ctree] =
    parseAll(prog, source) match {
      case Success(optree,_) => optree
      case _ => throw new Exception("Parse error!")
    }

  // Program P ::= CL
  def prog: Parser[List[Ctree]] = commlist

  // CommandList CL ::= C | C ; CL
  def commlist: Parser[List[Ctree]] = rep1sep(comm, ";")

  // Command C ::= D | L = E | print L | while E : CL end
  def comm: Parser[Ctree] =
    left~("="~>expr) ^^ { case l~e => Assign(l,e) } |
    "print"~>left ^^ { case l => Print(l) } |
    ("while"~>expr<~":")~(commlist<~"end") ^^ { case e~cs => While(e,cs) } |
      decl

  // Expression E ::= N | ( E1 + E2 ) | L | &L
  def expr: Parser[Etree] =
    wholeNumber ^^ (Num(_)) |
   "("~>expr~op~expr<~")" ^^ {
      case e1~"+"~e2 => Add(e1,e2)
      case e1~"-"~e2 => Sub(e1,e2)
   } |
   left ^^ (At(_)) |
   "&"~>left ^^ (Amph(_))

  def decl: Parser[Ctree] =
    "int"~>ident ^^ (Decl(_)) |
      ("int"~>"["~>wholeNumber<~"]")~ident^^{case n~l => Arr(n.toInt,l)}

  // LefthandSide L ::= I | *L | I[N]
  def left: Parser[Ltree] =
    ident~("["~>expr<~"]") ^^ {case l~exp => GetArr(l, exp)} |
      ident ^^ (Var(_)) |
      "*"~>left ^^ (Star(_))


  def op: Parser[String] = "+" | "-"
  // Interpreter
  val memory = scala.collection.mutable.ArrayBuffer.empty[Int]
  var env = Map.empty[String,Int]

  def interpretPTREE(p: List[Ctree]): Unit = interpretCLIST(p)

  def interpretCLIST(cs: List[Ctree]): Unit =
    for (c <- cs) yield interpretCTREE(c)

  def interpretCTREE(c: Ctree): Unit = c match {
    case Assign(l, e) => {
      val lval = interpretLTREE(l)
      val exprval = interpretETREE(e)
      memory(lval) = exprval
    }
      case Print(l) => {
      val loc = interpretLTREE(l)
      println(memory(loc))
    }
    case While(e, cs) => {
      val cond = interpretETREE(e)
      if (cond != 0) {
        interpretCLIST(cs)
        interpretCTREE(c)
      }
    }
    case Decl(x) => {
      if (env contains x) throw new Exception("variable " ++ x ++ " redeclared")
      else{
        memory += 0 // add a cell at the end of memory
        env += (x -> (memory.length - 1)) // save type and location for x
      }
    }
    case Arr(n,l) => {
      var num = 1;
      var loc = memory.length
      for(num<-1 to n)
      {
        memory += 0
      }
      env += (l->loc)
    }

  }

  def interpretETREE(e: Etree): Int = e match {
    case Num(n) => n.toInt
    case Add(e1,e2) => interpretETREE(e1) + interpretETREE(e2)
    case Sub(e1,e2) => interpretETREE(e1) - interpretETREE(e2)
    case At(l) => memory(interpretLTREE(l))
    case Amph(l) => interpretLTREE(l)
  }

  def interpretLTREE(l: Ltree): Int = l match {
    case Var(x) => {
      if (!(env contains x)) {
        // it is a brand new variable, so allocate a memory cell for it
        val newloc = memory.length
        memory += 0 // add a cell at the end of memory
        env += (x -> newloc) // remember the location
      }
      env(x) // look up its location
    }
    case Star(l) => { // a pointer dereference
      val loc = interpretLTREE(l) // get a location number
      memory(loc) // dereference it and return the location therein
    }
    case GetArr(l,exp) => {
      var nth = interpretETREE(exp)
      env(l) + nth
    }
  }
  //"int x; int[4] r; x=2; r[x] = (x+1); int y"
  //"int[4] r; r[0] = 0; r[1] = 1; r[2] = 2; r[3] = 3"
  //"int[4] r; r[0] = 0; r[1] = 1; r[2] = 2; r[3] = 3; print r[3]"
  // Controller
  def main(args: Array[String]): Unit = {
    try {
      val source = args(0)
      println("input : " + source)
      val optree = parse(source)
      println("optree : " + optree)
      interpretPTREE(optree)
      println("final memory : " + memory)
      println("final namespace : " + env)
    }
    catch { case e: Exception => println(e) }
  }
}
