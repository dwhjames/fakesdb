package fakesdb

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader.EofCh

case class SelectEval(output: OutputEval, from: String, where: WhereEval, order: OrderEval, limit: LimitEval)  {
  def select(data: Data, nextToken: Option[Int] = None): (List[(String, List[(String,String)])], Int, Boolean) = {
    val domain = data.getDomain(from).getOrElse(sys.error("Invalid from "+from))
    val drop = new SomeDrop(nextToken getOrElse 0)
    val (items, hasMore) = limit.limit(drop.drop(order.sort(where.filter(domain, domain.getItems.toList))))
    (output.what(domain, items), items.length, hasMore)
  }
}

sealed abstract class OutputEval {
  type OutputList = List[(String, List[(String, String)])]
  def what(domain: Domain, items: List[Item]): OutputList

  protected def flatAttrs(attrs: Iterator[Attribute]): List[(String, String)] = {
    attrs.flatMap((a: Attribute) => a.getValues.map((v: String) => (a.name, v))).toList
  }
}

case class CompoundOutput(attrNames: List[String]) extends OutputEval {
  def what(domain: Domain, items: List[Item]): OutputList = {
    items.map((item: Item) => {
      var i = (item.name, flatAttrs(item.getAttributes.filter((a: Attribute) => attrNames.contains(a.name))))
      if (attrNames.contains("itemName()")) { // ugly
        i = (i._1, ("itemName()", item.name) :: i._2)
      }
      i
    }).filter(_._2.size > 0)
  }
}

case object AllOutput extends OutputEval {
  def what(domain: Domain, items: List[Item]): OutputList = {
    items.map((item: Item) => {
      (item.name, flatAttrs(item.getAttributes))
    }).filter(_._2.size > 0)
  }
}

case object CountOutput extends OutputEval {
  def what(domain: Domain, items: List[Item]): OutputList = {
    List(("Domain", List(("Count", items.size.toString))))
  }
}

sealed abstract class WhereEval {
  def filter(domain: Domain, items: List[Item]): List[Item]

  protected def getFunc(op: String): Function2[String, String, Boolean] = op match {
    case "=" => _ == _
    case "!=" => _ != _
    case ">" => _ > _
    case "<" => _ < _
    case ">=" => _ >= _
    case "<=" => _ <= _
    case "like" => (v1, v2) => v1.matches(v2.replaceAll("%", ".*"))
    case "not-like" => (v1, v2) => !v1.matches(v2.replaceAll("%", ".*"))
  }
}

case object NoopWhere extends WhereEval {
  def filter(domain: Domain, items: List[Item]): List[Item] = items
}

case class SimpleWhereEval(name: String, op: String, value: String) extends WhereEval {
  def filter(domain: Domain, items: List[Item]): List[Item] = {
    val func = getFunc(op)
    items.filter((i: Item) => i.getAttribute(name) match {
      case Some(a) => a.getValues.find(func(_, value)).isDefined
      case None => false
    }).toList
  }
}

sealed abstract class LimitEval {
  def limit(items: List[Item]): (List[Item], Boolean)
}

case object NoopLimit extends LimitEval {
  def limit(items: List[Item]) = (items, false)
}

case class SomeLimit(limit: Int) extends LimitEval {
  def limit(items: List[Item]) = (items take limit, items.size > limit)
}

case class SomeDrop(count: Int) {
  def drop(items: List[Item]) = items drop count
}

case class EveryEval(name: String, op: String, value: String) extends WhereEval {
  override def filter(domain: Domain, items: List[Item]): List[Item] = {
    val func = getFunc(op)
    items.filter((i: Item) => i.getAttribute(name) match {
      case Some(a) => a.getValues.forall(func(_, value))
      case None => false
    }).toList
  }
}

case class CompoundWhereEval(sp: WhereEval, op: String, rest: WhereEval) extends WhereEval {
  def filter(domain: Domain, items: List[Item]): List[Item] = {
    op match {
      case "intersection" => sp.filter(domain, items).toList intersect rest.filter(domain, items).toList
      case "and" => sp.filter(domain, items).toList intersect rest.filter(domain, items).toList
      case "or" => sp.filter(domain, items).toList union rest.filter(domain, items).toList
      case _ => sys.error("Invalid operator "+op)
    }
  }
}

case class IsNullEval(name: String, isNull: Boolean) extends WhereEval {
  def filter(domain: Domain, items: List[Item]): List[Item] = {
    items.filter((i: Item) => if (isNull) {
      i.getAttribute(name).isEmpty
    } else {
      i.getAttribute(name).isDefined
    }).toList
  }
}

case class IsBetweenEval(name: String, lower: String, upper: String) extends WhereEval {
  def filter(domain: Domain, items: List[Item]): List[Item] = {
    items.filter((i: Item) => i.getAttribute(name) match {
      case Some(a) => a.getValues.exists(_ >= lower) && a.getValues.exists(_ <= upper)
      case None => false
    }).toList
  }
}

case class InEval(name: String, values: List[String]) extends WhereEval {
  def filter(domain: Domain, items: List[Item]): List[Item] = {
    items.filter((i: Item) => i.getAttribute(name) match {
      case Some(a) => a.getValues.exists(values.contains(_))
      case None => false
    }).toList
  }
}

sealed abstract class OrderEval {
  def sort(items: List[Item]): List[Item]
}

case object NoopOrder extends OrderEval {
  def sort(items: List[Item]) = items
}

case class SimpleOrderEval(name: String, way: String) extends OrderEval {
  def sort(items: List[Item]): List[Item] = {
    val comp = (lv: String, rv: String) => way match {
      case "desc" => lv > rv
      case _ => lv < rv
    }
    items.sortWith((l, r) => {
      comp(resolveValue(l), resolveValue(r))
    })
  }
  def resolveValue(item: Item) = {
    if (name == "itemName()") {
      item.name
    } else {
      item.getAttribute(name) match {
        case Some(a) => a.getValues.next
        case None => "" // default value
      }
    }
  }
}

class SelectLexical extends StdLexical {
  /*
   * Attribute and domain names may appear without quotes if they contain
   * only letters, numbers, underscores (_), or dollar symbols ($) and do
   * not start with a number. You must quote all other attribute and domain
   * names with the backtick (`).
   */
  override def token: Parser[Token] =
    ( accept("itemName()".toList)             ^^^ { Identifier("itemName()") }
    | accept("count(*)".toList)               ^^^ { Keyword("count(*)") }
    | '\'' ~> rep(chrEscapeTicks)     <~ '\'' ^^  { chrs => StringLit(chrs.mkString) }
    | '"'  ~> rep(chrEscapeQuotes )   <~ '"'  ^^  { chrs => StringLit(chrs.mkString) }
    | '`'  ~> rep(chrEscapeBackTicks) <~ '`'  ^^  { chrs => Identifier(chrs.mkString) }
    | rep1(identChar, identChar | digit)      ^^  { chrs => processIdent(chrs.mkString) }
    | rep1(digit)                             ^^  { chrs => NumericLit(chrs.mkString) }
    | EofCh                                   ^^^ EOF
    | delim
    | failure("illegal character")
    )

  // Add $ to letters and _ as acceptable first characters of unquoted identifiers
  override def identChar = letter | elem('_') | elem('$')

  def chrEscapeTicks = ('\'' ~ '\'') ^^^ '\'' | chrExcept('\'', EofCh)

  def chrEscapeQuotes = ('"' ~ '"') ^^^ '"' | chrExcept('"', EofCh)

  def chrEscapeBackTicks = ('`' ~ '`') ^^^ '`' | chrExcept('`', EofCh)
}

object SelectParser extends StandardTokenParsers {
  override val lexical = new SelectLexical
  lexical.delimiters ++= List("*", ",", "=", "!=", ">", "<", ">=", "<=", "(", ")")
  lexical.reserved ++= List(
    "select", "from", "where", "and", "or", "like", "not", "is", "null", "between",
    "every", "in", "order", "by", "asc", "desc", "intersection", "limit", "count(*)"
  )

  def expr =
    (  ("select" ~> outputList)
    ~! ("from" ~> ident)
    ~! whereClause
    ~! order
    ~! limit
         ^^ { case ol ~ i ~ w ~ o ~ l => SelectEval(ol, i, w, o, l) }
    )

  def order: Parser[OrderEval] =
    ( "order" ~> "by" ~> ident ~! opt("asc" | "desc")
        ^^ { case i ~ way => SimpleOrderEval(i, way.getOrElse("asc")) }
    | success(NoopOrder)
    )

  def limit: Parser[LimitEval] =
    ( "limit" ~> numericLit ^^ { num => SomeLimit(num.toInt) }
    | success(NoopLimit)
    )

  def whereClause: Parser[WhereEval] =
    ( "where" ~> where
    | success(NoopWhere)
    )

  def setOp = "and" | "or" | "intersection"

  def where: Parser[WhereEval] =
    ( simplePredicate ~! opt(setOp ~! where)
        ^^ { case sp ~ opt_rest => opt_rest match {
               case Some(op ~ rp) => CompoundWhereEval(sp, op, rp)
               case None          => sp } } )

  def op = "=" | "!=" | ">" | "<" | ">=" | "<=" | "like" | "not" ~ "like" ^^^ { "not-like" }

  def simplePredicate: Parser[WhereEval] =
    ( "(" ~> where <~ ")"
    | ("every" ~> "(" ~> ident <~ ")") ~! op ~! stringLit
        ^^ { case i ~ o ~ v => EveryEval(i, o, v)}
    | ident ~ ("is" ~> ( "null"          ^^^ { IsNullEval(_: String, true) }
                       | "not" ~! "null" ^^^ { IsNullEval(_: String, false) }
                       )
              | comparisonOp
              ) ^^ { case i ~ f => f(i)}
    )

  def comparisonOp: Parser[Function1[String,WhereEval]] =
    ( ("between" ~> stringLit) ~! ("and" ~> stringLit)
        ^^ { case a ~ b => IsBetweenEval(_: String, a, b) }
    | ("in" ~> "(" ~> repsep(stringLit, ",") <~ ")")
        ^^ { strs => InEval(_: String, strs) }
    | op ~! stringLit
        ^^ { case o ~ v => SimpleWhereEval(_: String, o, v) }
    )

  def outputList: Parser[OutputEval] =
    ( "*"                ^^^ { AllOutput }
    | "count(*)"         ^^^ { CountOutput }
    | repsep(ident, ",") ^^  { attrNames => CompoundOutput(attrNames) }
    )

  def makeSelectEval(input: String): SelectEval = {
    val tokens = new lexical.Scanner(input)
    phrase(expr)(tokens) match {
      case Success(selectEval, _) => selectEval
      case Failure(msg, _) => sys.error(msg)
      case Error(msg, _) => sys.error(msg)
    }
  }
}
