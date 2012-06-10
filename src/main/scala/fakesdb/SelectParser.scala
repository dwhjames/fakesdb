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

sealed abstract class Predicate extends Function1[String, Boolean] {
  /** Lifted logical and */
  def &&(that: Predicate)= new Predicate {
    def apply(v: String): Boolean = this(v) && that(v)
  }
  /** Lifted logical or */
  def ||(that: Predicate) = new Predicate {
    def apply(v: String): Boolean = this(v) || that(v)
  }
}

case class BinOpPredicate(op: String, value: String) extends Predicate {
  import java.util.regex.Pattern
  val predicate: Function1[String, Boolean] = op match {
    case "="        => _ == value
    case "!="       => _ != value
    case ">"        => _ > value
    case "<"        => _ < value
    case ">="       => _ >= value
    case "<="       => _ <= value
    case "like"     => compilePattern(value).matcher(_).matches
    case "not-like" => !compilePattern(value).matcher(_).matches
  }
  protected def compilePattern(str: String): Pattern = {
    import scala.collection.mutable.StringBuilder
    var start = 0
    var end = str.length
    val pat = new StringBuilder(end)
    if (str.startsWith("%")) {
      pat ++= ".*"
      start += 1
    }
    pat ++= "\\Q"
    val pcntAtEnd = !str.endsWith("\\%") && str.endsWith("%")
    if (pcntAtEnd) end -= 1
    pat ++= str.substring(start, end)
    pat ++= "\\E"
    if (pcntAtEnd) pat ++= ".*"
    Pattern.compile(pat.toString)
  }
  def apply(v: String): Boolean = predicate(v)
}

case class RangePredicate(lower: String, upper: String) extends Predicate {
  def apply(v: String): Boolean = (v >= lower) && (v <= upper)
}

case class ContainsPredicate(values: Set[String]) extends Predicate {
  def apply(v: String): Boolean = values.contains(v)
}

sealed abstract class WhereEval {
  def filter(domain: Domain, items: List[Item]): List[Item]
}

case object NoopWhere extends WhereEval {
  def filter(domain: Domain, items: List[Item]): List[Item] = items
}

case class ExistsEval(name: String, pred: Predicate) extends WhereEval {
  def filter(domain: Domain, items: List[Item]): List[Item] = {
    items.filter((i: Item) => i.getAttribute(name) match {
      case Some(a) => a.getValues.exists(pred(_))
      case None => false
    }).toList
  }
}

case class EveryEval(name: String, pred: Predicate) extends WhereEval {
  override def filter(domain: Domain, items: List[Item]): List[Item] = {
    items.filter((i: Item) => i.getAttribute(name) match {
      case Some(a) => a.getValues.forall(pred(_))
      case None => false
    }).toList
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
    | ("every" ~> "(" ~> ident <~ ")") ~! comparisonOp
        ^^ { case i ~ p => EveryEval(i, p)}
    | ident ~! ("is" ~> ( "null"          ^^^ { IsNullEval(_: String, true) }
                        | "not" ~! "null" ^^^ { IsNullEval(_: String, false) }
                        )
               | comparisonOp ^^ { pred => ExistsEval(_:String, pred) }
               ) ^^ { case i ~ f => f(i)}
    )

  def comparisonOp: Parser[Predicate] =
    ( ("between" ~> stringLit) ~! ("and" ~> stringLit)
        ^^ { case lower ~ upper => RangePredicate(lower, upper) }
    | ("in" ~> "(" ~> repsep(stringLit, ",") <~ ")")
        ^^ { strs => ContainsPredicate(strs.toSet) }
    | op ~! stringLit
        ^^ { case o ~ v => BinOpPredicate(o, v) }
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
