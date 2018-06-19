// Bubble Sort Interpreter for a C-like mini-language
//
// Program P ::= DL ; CL
// CommandList CL ::= C | C ; CL
// DeclareList DL ::= D | D ; DL
// ExpressionList EL ::= E | E , EL
// Command C ::= L = E | if M { CL1 else CL2 } | while M { CL } | print L
// Expression E ::= N | L | (E1 < E2) | (E1 == E2) | (E1 + E2) | (E1 - E2) | (E1 * E2) | (E1 / E2) |[EL]
// Declare D ::= var I = E
// LefthandSide L ::= I | L[E]
// Numeral N ::= 0 | 1 | 2 | ...
// Identifier I ::= strings of letters, not including keywords: while, print, end

// Operator Tree
// PTREE ::= List(DTREE) ; List(CTREE)
// CTREE ::= Assign(LTREE,ETREE) | If(ETREE,List[CTREE],List[CTREE]) | While(ETREE,List(CTREE)) | Print(LTREE)
// ETREE ::= Num(String) | At(LTREE) |  Add(ETREE,ETREE) | Sub(ETREE,ETREE) | Mul(ETREE,ETREE) | Div(ETREE,ETREE) | Arr(List(ETREE))
// DTREE ::= Decl(STRING,ETREE)
// LTREE ::= Var(String) | GetArr(String,ETREE)
// MTREE ::= Compare(ETREE,ETREE) | Equal(ETREE,ETREE)

import scala.util.parsing.combinator.JavaTokenParsers

trait OpTree {
  sealed abstract class Rval {
    override def toString(): String = {
      this match {
        case Handle(l) => "`" + l
        case Value(n) => n.toString()
        case Nil => "nil"
      }
    }
    // r1 + r2
    // r1 - r2
    // r1 * r2
    // r1 / r2
    def +(r:Rval): Rval = {
      this match {
        case Value(n1) =>
          r match {
            case Value(n2) => Value(n1 + n2)
            case _ => throw new Exception
          }
        case _ => throw new Exception
      }
    }
    def -(r:Rval): Rval = {
      this match {
        case Value(n1) =>
          r match {
            case Value(n2) => Value(n1 - n2)
            case _ => throw new Exception
          }
        case _ => throw new Exception
      }
    }
    def *(r:Rval): Rval = {
      this match {
        case Value(n1) =>
          r match {
            case Value(n2) => Value(n1 * n2)
            case _ => throw new Exception
          }
        case _ => throw new Exception
      }
    }
    def /(r:Rval): Rval = {
      this match {
        case Value(n1) =>
          r match {
            case Value(n2) => Value(n1 / n2)
            case _ => throw new Exception
          }
        case _ => throw new Exception
      }
    }
    def <(r:Rval): Boolean = {
      this match {
        case Value(n1) =>
          r match {
            case Value(n2) => {
              if(n1<n2)
                true
              else
                false
            }
            case _ => throw new Exception
          }
        case _ => throw new Exception
      }
    }
    def ==(r:Rval): Boolean = {
      this match {
        case Value(n1) =>
          r match {
            case Value(n2) => {
              if(n1 == n2)
                true
              else
                false
            }
            case _ => throw new Exception
          }
        case _ => throw new Exception
      }
    }
  }
  case class Handle(loc: Int) extends Rval
  case class Value(n: Int) extends Rval
  case class BValue(b: Boolean) extends Rval
  case object Nil extends Rval

  sealed abstract class Ltree
  case class Var(i: String) extends Ltree
  case class GetArr(i:String, exp: Etree) extends Ltree

  sealed abstract class Etree
  case class Num(s: String) extends Etree
  case class At(l : Ltree) extends Etree
  case class Add(e1: Etree, e2: Etree) extends Etree
  case class Sub(e1: Etree, e2: Etree) extends Etree
  case class Mul(e1: Etree, e2: Etree) extends Etree
  case class Div(e1: Etree, e2: Etree) extends Etree
  case class Arr(el: List[Etree]) extends Etree

  sealed abstract class Ctree
  case class Assign(l: Ltree, e: Etree) extends Ctree
  case class If(c: Mtree, t: List[Ctree], f: List[Ctree]) extends Ctree
  case class While(c : Mtree, cl : List[Ctree]) extends Ctree
  case class Print(e: Etree) extends Ctree

  sealed abstract class Dtree
  case class Decl(i:String, e:Etree) extends Dtree

  sealed abstract class Mtree
  case class Compare(e1: Etree, e2: Etree) extends Mtree
  case class Equal(e1: Etree, e2: Etree) extends Mtree
}

object Bubble extends JavaTokenParsers with OpTree {

  //Parser
  def parse(source: String): (List[Dtree],List[Ctree]) =
    parseAll(prog, source) match {
      case Success(optree,_) => optree
      case _ => throw new Exception("Parse error!")
    }

//   Program P ::= CL
  def prog: Parser[(List[Dtree],List[Ctree])] =
    declist~";"~commlist^^{case dl~";"~cl => (dl,cl)}

  // CommanList CL ::= C | C ; CL
  def commlist: Parser[List[Ctree]] = rep1sep(comm,";")

  // DeclareList DL ::= D | D ; DL
  def declist: Parser[List[Dtree]] = rep1sep(decl,";")

  // ExpressionList EL ::= E | E , EL
  def exprlist: Parser[List[Etree]] = rep1sep(expr,",")

  // Command C ::= L = E | if M : CL1 else CL2 end | while M : CL end | print L
  def comm: Parser[Ctree] =
    left~("="~>expr) ^^ { case l~e => Assign(l,e) } |
      "print"~>expr ^^ {case e => Print(e)} |
      ("if"~>cond)~("{"~>commlist)~("else"~>commlist<~"}") ^^
        { case c~t~f => If(c,t,f) } |
      ("while"~>cond)~("{"~>commlist<~"}") ^^ {case c~cl => While(c,cl)}

  // Declare D ::= var I = E | var I
  def decl: Parser[Dtree] =
  ("var"~>ident)~("="~>expr) ^^ { case i~e => Decl(i,e)} |
  "var"~>ident ^^ { case i => Decl(i,Num("0"))}

  // Expression E ::= N | L | E1 < E2 | E1 == E2 | E1 + E2 | E1 - E2 | E1 * E2 | E1 / E2 | [EL]
  def expr: Parser[Etree] =
    wholeNumber ^^ (Num(_)) |
      left ^^ (At(_)) |
      "["~>exprlist<~"]" ^^ {case el => Arr(el)} |
      "("~>expr~op~expr<~")" ^^ {
        case e1~"+"~e2 => Add(e1,e2)
        case e1~"-"~e2 => Sub(e1,e2)
        case e1~"*"~e2 => Mul(e1,e2)
        case e1~"/"~e2 => Div(e1,e2)
      }

  def cond: Parser[Mtree] =
    "("~>expr~compare~expr<~")" ^^ {
      case e1~"<"~e2 => Compare(e1,e2)
      case e1~"=="~e2 => Equal(e1,e2)
    }

  // LefthandSide L ::= I | L[E]
  def left: Parser[Ltree] =
    ident~("["~>expr<~"]") ^^ {case l~exp => GetArr(l, exp)} |
      ident ^^ (Var(_))

  def op: Parser[String] = "+" | "-" | "*" | "/" | "<" | "=="
  def compare : Parser[String] = "<" | "=="

  var heap : Map[Handle, Map[String,Rval]] = Map()

  def allocateNS(): Handle = {
    var newhandle = Handle(heap.size)
    heap += (newhandle -> Map())
    newhandle
  }

  val ns = allocateNS()

  def lookup(lval: (Handle, String)): Rval = {
    val (handle, fieldname) = lval
    if ((heap contains handle) &&
      (heap(handle) contains fieldname))
      heap(handle)(fieldname)
    else
      throw new Exception("lookup error: " + handle)
  }

  def store(lval: (Handle, String), rval: Rval): Unit = {
    val (handle, fieldname) = lval
    if (heap contains handle) {
      heap += (handle -> (heap(handle) + (fieldname -> rval)))
    } else // if the handle is not in the heap
      throw new Exception("store error: " + handle)
  }

  def PTREEtoStr(p: (List[Dtree],List[Ctree])): String = {
    val (dl,cl) = p
    var str = ""
    dl match {
      case h :: t => t.foldLeft(str)(_ + ";" + _)
      case _ => ""
    }
    cl match {
      case h :: t => t.foldLeft(str)(_ + ";" + _)
      case _ => ""
    }
    str
  }

  // Operator Tree
  // PTREE ::= List(DTREE) ; List(CTREE)
  // CTREE ::= Assign(LTREE,ETREE) | If(ETREE,List[CTREE],List[CTREE]) | While(ETREE,List(CTREE)) | Print(LTREE)
  // ETREE ::= Num(String) | At(LTREE) |  Add(ETREE,ETREE) | Sub(ETREE,ETREE) | Mul(ETREE,ETREE) | Div(ETREE,ETREE) | Arr(List(ETREE))
  // DTREE ::= Decl(STRING,ETREE)
  // LTREE ::= Var(String) | GetArr(String,ETREE)
  // MTREE ::= Compare(ETREE,ETREE) | Equal(ETREE,ETREE)

  def interpretPTREE(p: (List[Dtree],List[Ctree])): Unit = {
    val (ds,cs) = p
    interpretDLIST(ds)
    interpretCLIST(cs)
  }

  def interpretDLIST(ds: List[Dtree]): Unit =
    for(d <- ds) yield interpretDTREE(ns,d)

  def interpretCLIST(cs: List[Ctree]): Unit =
    for (c <- cs) yield interpretCTREE(c)

  def interpretCTREE(c: Ctree): Unit = c match {
    case Assign(l,e) => {
      val lval = interpretLTREE(l)
      val rval = interpretETREE(e)
      store(lval, rval)
    }
    case If(e,t,f) => {
      if(interpretMTREE(e)){
        interpretCLIST(t)
      }else{
        interpretCLIST(f)
      }
    }
    case While(e,cs) => {
      while(interpretMTREE(e)){
        interpretCLIST(cs)
      }
    }
    case Print(e) => {
      println(interpretETREE(e))
    }
  }

  def interpretETREE(e: Etree): Rval = e match {
    case Num(n) => Value(n.toInt)
    case At(l) => {
      lookup(interpretLTREE(l))
    }
    case Add(e1,e2) => {interpretETREE(e1) + interpretETREE(e2)}
    case Sub(e1,e2) => {interpretETREE(e1) - interpretETREE(e2)}
    case Mul(e1,e2) => {interpretETREE(e1) * interpretETREE(e2)}
    case Div(e1,e2) => {interpretETREE(e1) / interpretETREE(e2)}
    case Arr(el) => {
      val newhandle = allocateNS()
      var i = 0;
      for (e <- el){
        val x = interpretETREE(e)
        store((newhandle,i.toString),x)
        i = i +1
      }
      newhandle
    }
  }

  def interpretMTREE(m : Mtree) : Boolean = m match{
    case Compare(e1,e2) =>{
      interpretETREE(e1) < interpretETREE(e2)
    }
    case Equal(e1,e2) => {
      interpretETREE(e1) == interpretETREE(e2)
    }
  }

  def interpretLTREE(l: Ltree): (Handle, String) = l match {
    case Var(i) => (ns,i)
    case GetArr(l,exp) => {
      val nth = interpretETREE(exp)
      var id = interpretLTREE(Var(l))
      val handle = lookup(id) match {
        case Handle(loc) => Handle(loc)
        case _ => throw new Exception("Will never be selected.")
      }
      (handle,nth.toString())
    }
  }

  def interpretDTREE(handle : Handle, d : Dtree): Unit = d match {
    case Decl(i,e) => {
      val rval = interpretETREE(e)
      store((handle,i),rval)
    }
  }

  // Bubble Sort Interpreter for a C-like mini-language
  //
  // Program P ::= DL ; CL
  // CommandList CL ::= C | C ; CL
  // DeclareList DL ::= D | D ; DL
  // ExpressionList EL ::= E | E , EL
  // Command C ::= L = E | if M { CL1 else CL2 } | while M { CL } | print L
  // Expression E ::= N | L | (E1 < E2) | (E1 == E2) | (E1 + E2) | (E1 - E2) | (E1 * E2) | (E1 / E2) |[EL]
  // Declare D ::= var I = E
  // LefthandSide L ::= I | L[E]
  // Numeral N ::= 0 | 1 | 2 | ...
  // Identifier I ::= strings of letters, not including keywords: while, print, end

  // Controller
  def main(args : Array[String]): Unit = {
    try {
      val source = args(0)
      println("input : " + source)
      val optree = parse(source)
      println("optree : " + optree)
      interpretPTREE(optree)
      println("final heap : " + heap)
    }
    catch { case e: Exception => println(e) }
  }

}

