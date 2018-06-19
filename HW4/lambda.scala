import scala.util.parsing.combinator.JavaTokenParsers

trait OpTree {

  sealed abstract class Etree

  case class Lambda(i:String, e : Etree ) extends Etree

  case class Lambdaapp(i:String, e : Etree ) extends Etree

  case class Atom(s: String) extends Etree

  case class Cons(e1: Etree, e2: Etree) extends Etree

  case class Head(e: Etree) extends Etree

  case class Tail(e: Etree) extends Etree

  case class IfNil(e1: Etree, e2: Etree, e3: Etree) extends Etree

  case class Let(d: Dtree, e: Etree) extends Etree

  case class Ref(id: String) extends Etree

  case object Nil extends Etree


  sealed abstract class Dtree

  case class Def(id: String, e: Etree) extends Dtree


  sealed abstract class HeapContents

  case class NameSpace(ns: Map[Name, Denotable]) extends HeapContents

  case class ConsPair(head: Denotable, tail: Denotable) extends HeapContents


  sealed abstract class Name

  case class Id(s: String) extends Name

  case object ParentNS extends Name


  sealed abstract class Denotable

  case class Handle(loc: Int) extends Denotable

  case class Value(a: String) extends Denotable

  case object Null extends Denotable

  var heap: Map[Handle, HeapContents] = Map()

}

object lambda extends JavaTokenParsers with OpTree {

  def parse(source: String): Etree =
    parseAll(exprlist, source) match {
      case Success(optree, _) => optree
      case _ => throw new Exception("Parse error!")
    }

  def exprlist: Parser[Etree] =
    expr~("::"~>exprlist) ^^ {case e1~e2 => Cons(e1,e2)} |
      expr

  def expr: Parser[Etree] =
    ("("~"lambda"~"(")~>ident~(")"~>(exprlist<~")")) ^^ { case i~el => Lambda(i,el)}|
      ("let" ~> define) ~ ("in" ~> exprlist <~ "end") ^^ {
        case d~e => Let(d,e)
      }|
      stringLiteral ^^ Atom|
      "nil" ^^ (_=>Nil)|
      "hd"~("("~> exprlist<~")") ^^ {
        case _~e => Head(e)
      } |
      "tl"~("("~> exprlist<~")") ^^ {
        case _~e => Tail(e)
      } |
      ("ifnil" ~> expr <~ "then") ~ (expr <~ "else") ~ expr ^^ {
        case e1~e2~e3 => IfNil(e1, e2, e3)
      } |
      ident~("("~>exprlist<~")") ^^ {
        case i~e => Lambdaapp(i,e)
      }|
      ident ^^ {
        case ids => Ref(ids)
      }

  def define: Parser[Dtree] =
    ("val" ~> ident <~ "=") ~ exprlist ^^ {
      case id~e => Def(id, e)
    }

  def allocate(v: HeapContents): Handle = {
    val newloc: Int = heap.size
    heap += (Handle(newloc) -> v)
    Handle(newloc)
  }

  def lookupNS(h: Handle, n: Name): Denotable = {
    if (heap contains h)
      heap(h) match {
        case NameSpace(ns) =>
          if (ns contains n) ns(n)
          else
            ns(ParentNS) match {
              case Handle(h) => lookupNS(Handle(h): Handle, n)
              case d => throw new Exception("lookup error: " + d)
            }
        case ConsPair(_, _) =>
          throw new Exception("lookup error: " + h + " not a namespace")
      }
    else
      throw new Exception("lookup error: " + h + " does not exist in the heap")
  }

  def storeNS(h: Handle, n: Name, v: Denotable): Unit = {
    if (heap contains h) heap(h) match {
      case NameSpace(ns) =>
        if (ns contains n)
          throw new Exception("store error: the name " + n + " is already bound")
        else {
          val newns = ns + (n -> v)
          heap += (h -> NameSpace(newns))
        }
      case ConsPair(_, _) =>
        throw new Exception("store error: " + h + " not a namespace")
    }
    else // if the handle is not in the heap
      throw new Exception("store error: " + h + " does not exist in the heap")
  }

  def getConsPair(v: Denotable): (Denotable, Denotable) =
    v match {
      case Handle(h) =>
        heap(Handle(h)) match {
          case ConsPair(head, tail) => (head, tail)
          case _ => throw new Exception("error: value is not a cons pair")
        }
      case _ => throw new Exception("error: value is not a handle")
    }


  def evalEtree(e: Etree, ns: Handle): Denotable =
    e match {
      case Atom(s) => Value(s)
      case Nil => Null
      case Cons(e1, e2) =>
        val v1 = evalEtree(e1, ns)
        val v2 = evalEtree(e2, ns)
        allocate(ConsPair(v1, v2))
      case Head(e) =>
        val v = evalEtree(e, ns)
        val (head, _) = getConsPair(v)
        head
      case Tail(e) =>
        val v = evalEtree(e, ns)
        val (_, tail) = getConsPair(v)
        tail
      case IfNil(e1, e2, e3) =>
        evalEtree(e1, ns) match {
          case Null => evalEtree(e2, ns)
          case _ => evalEtree(e3, ns)
        }
      case Let(d, e) =>
        val newns = evalDtree(d, ns)
        evalEtree(e, newns)
      case Ref(s) =>
        lookupNS(ns, Id(s))
    }

  def evalDtree(d: Dtree, ns: Handle): Handle =
    d match {
      case Def(s, e) =>
        val newns = allocate(NameSpace(Map(ParentNS -> ns)))
        val name = Id(s)
        val value = evalEtree(e, ns)
        storeNS(newns, name, value)
        newns
    }

  def main(args: Array[String]): Unit = {
    try {
      val source = args(0)
      val optree = parse(source)
      println("optree : " + optree)
      val answer = evalEtree(optree, Handle(-1))
      println("answer : " + answer)
      println("heap : " + heap)
    }
    catch {
      case e: Exception => println(e)
    }
  }
  //"\"abc\" :: nil"
  //"hd (\"abc\" :: nil)"
  //"ifnil nil then hd (\"abc\" :: nil) else \"def\""
  //"let val x = \"abc\" in x :: nil end"
  //"let val x = \"abc\" :: nil in x :: x end"
  //"let val x = \"abc\" :: nil in let val y = nil in x :: y end end"
  //"(lambda (list) let val rest = tl (list) in hd (rest) end)"
  //"let val second = (lambda(list)let val rest = tl (list) in hd (rest) end) in second(tl(\"a\"::\"b\"::\"c\"::nil)) end"
}

