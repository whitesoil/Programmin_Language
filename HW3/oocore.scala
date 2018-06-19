
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

    // r1.+(r2)
    // r1 + r2
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
  }
  case class Handle(loc: Int) extends Rval
  case class Value(n: Int) extends Rval
  case class BValue(b: Boolean) extends Rval
  case object Nil extends Rval

  sealed abstract class Ltree {
    override def toString(): String = {
      this match {
        case Id(x) => x
        case Dot(ns, x) => "" + ns + "." + x
      }
    }
  }
  case class Id(x: String) extends Ltree
  case class Dot(ns: Ltree, x:Ltree) extends Ltree

  sealed abstract class Etree {
    override def toString(): String = {
      this match {
        case Num(s) => s
        case Add(e1, e2) => "" + e1 + " + " + e2
        case Deref(l) => l.toString()
        case New(os) =>
          os match {
            case h :: t => "new { " + t.foldLeft(h)(_ + "," + _) + " }"
            case _ => "new {}"
          }
        case NewT(t) => "new " + t
      }
    }
  }
  case class Num(s: String) extends Etree
  case class Add(e1: Etree, e2: Etree) extends Etree
  case class Deref(l: Ltree) extends Etree
  case class New(o: List[String]) extends Etree
  case class NewT(t:Ttree) extends Etree

  sealed abstract class Ctree {
    override def toString(): String = {
      this match {
        case Assign(l, e) => "" + l + " = " + e
        case Cond(e,ct,cf) =>
          "if " + e + " : " +
            (ct match {
              case h :: t => t.foldLeft(h.toString())(_ + ";" + _)
              case _ => ""
            }) +
            " else " +
            (cf match {
              case h :: t => t.foldLeft(h.toString())(_ + ";" + _)
              case _ => ""
            }) +
            " end"
        case Print(l) => "print " + l
      }
    }
  }
  case class Assign(l: Ltree, e: Etree) extends Ctree
  case class Cond(e: Etree, ct: List[Ctree], cf: List[Ctree]) extends Ctree
  case class Print(L: Ltree) extends Ctree
  case class Seq(c1: Ctree, c2: Ctree) extends Ctree

  sealed abstract class Dtree{
    override def toString(): String = {
      this match{
        case Var(i,e) => "var " + i +" = " + e
      }
    }
  }
  case class Var(i:String, e:Etree) extends Dtree

  sealed abstract class Ttree{
    override def toString(): String = {
      this match{
        case Struct(dl) =>
          dl match {
            case h :: t => t.foldLeft(h.toString())(_ + ";" + _)
            case _ => ""
          }
        case Arr(n,e) => "array["+ n +"] of " + e
      }
    }
  }
  case class Struct(dl: List[Dtree]) extends Ttree
  case class Arr(n:String,e:Etree) extends Ttree
}



object BabyOO extends JavaTokenParsers with OpTree {
  // Parser
  def parse(source: String): (List[Dtree],List[Ctree]) =
    parseAll(prog, source) match {
      case Success(optree,_) => optree
      case _ => throw new Exception("Parse error!")
    }

  // Program P ::= DL ; CL
  def prog: Parser[(List[Dtree],List[Ctree])] =
    declist~(";"~>commlist)^^{case dl~cl => (dl,cl)}

  // DeclarationList DL ::= D | D ; DL
  def declist: Parser[List[Dtree]] = rep1sep(dec,";")

  // CommandList CL ::= C | C ; CL
  def commlist: Parser[List[Ctree]] = rep1sep(comm, ";")

  // Command C ::= L = E | print L | if E : CL1 else CL2 end
  def comm: Parser[Ctree] =
    left~("="~>expr) ^^ { case l~e => Assign(l,e) } |
      "print"~>left ^^ { case l => Print(l) } |
      ("if"~>expr<~":")~(commlist<~"else")~(commlist<~"end") ^^
        { case e~ct~cf => Cond(e,ct,cf) }

  // Declaration D ::= var I = E | D1 ; D2
   def dec: Parser[Dtree] =
    ("var"~>ident<~"=")~expr ^^ {
      case id~e =>
        Var(id,e)}

  // Expression E ::= N | ( E1 + E2 ) | L | new { I,* } | new T
  def expr: Parser[Etree] =
    wholeNumber ^^ (Num(_)) |
      ("("~>expr)~("+"~>expr<~")") ^^ {
        case e1~e2 => Add(e1,e2)
      } |
      "new"~>templ^^{case t => NewT(t)}|
      "new"~("{"~>rep1sep(ident, ",")<~"}") ^^ {
        case _~ids => New(ids)
      }|
      left ^^ (Deref(_))


  // TypeTemplate T ::= struct D end | array[N] of E
  def templ: Parser[Ttree] =
    ("struct"~>declist<~"end")^^{case dl => Struct(dl)} |
      "array"~>("["~>wholeNumber<~"]")~("of"~>expr) ^^ { case n~e => Arr(n,e)}

  // LefthandSide L ::= I | L . I
  def left: Parser[Ltree] =
    rep1sep(ident, ".") ^^ {
      case ids =>
        val h :: t = ids
        t.foldLeft(Id(h):Ltree)((l, id) => Dot(l, Id(id)))
    }

  // Interpreter
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

  def interpretPTREE(p: (List[Dtree],List[Ctree])): Unit = {
    val (ds,cs) = p
    interpretDLIST(ds)
    interpretCLIST(cs)
  }

  def interpretDLIST(ds: List[Dtree]): Unit =
    for(d <- ds) yield interpretDTREE(ns,d)

  def interpretCLIST(cs: List[Ctree]): Unit =
    for (c <- cs) yield interpretCTREE(c)

  def interpretDTREE(handle : Handle, d : Dtree) : Unit = d match {
    case Var(i,e) => {
      val rval = interpretETREE(e)
      store((handle,i),rval)
    }
  }

  def interpretCTREE(c: Ctree): Unit = c match {
    case Assign(l, e) => {
      val lval = interpretLTREE(l)
      val rval = interpretETREE(e)
      store(lval, rval)
    }
    case Cond(e, ct, cf) => {
      interpretETREE(e) match {
        case Value(n) =>
          if (n == 0) interpretCLIST(cf)
          else interpretCLIST(ct)
        case _ => throw new Exception
      }
    }
    case Print(l) => {
      println(lookup(interpretLTREE(l)))
    }
  }

  def interpretETREE(e: Etree): Rval = e match {
    case Num(n) => Value(n.toInt)
    case Add(e1,e2) =>
      val r1 = interpretETREE(e1)
      val r2 = interpretETREE(e2)
      r1 + r2
    case Deref(l) =>
      lookup(interpretLTREE(l))
    case New(fields) =>
      val newhandle = allocateNS()
      for (f <- fields) yield
        store((newhandle,f), Nil)
      newhandle
    case NewT(t) =>
      val newhandle = interpretTTREE(t)
      newhandle
  }

  def interpretLTREE(left: Ltree): (Handle, String) = left match {
    case Id(x) => (ns, x)
    case Dot(ls, Id(x)) => {
      val lval = interpretLTREE(ls)
      val handle = lookup(lval) match {
        case Handle(loc) => Handle(loc)
        case _ => throw new Exception("Will never be selected.")
      }
      (handle, x)
    }
    case _ => throw new Exception("Will never be selected.")
  }

  def interpretTTREE(t : Ttree): Handle = t match{
    case Struct(dl) => {
      val newhandle = allocateNS()
      for(d<-dl) yield
        interpretDTREE(newhandle,d)
      newhandle
    }
    case Arr(n,e) => {
      val newhandle = allocateNS()
      val expr = interpretETREE(e)
      for(i <- 0 to n.toInt-1){
        store((newhandle,i.toString),expr)
      }
      newhandle
    }
  }

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
