package fakesdb

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader.EofCh

case class SelectQuery(output: OutputClause, from: String, where: ItemPredicate, order: OrderClause, limit: LimitClause)  {
  def select(data: Data, nextToken: Option[Int] = None): (Seq[(String, Seq[(String,String)])], Int, Boolean) = {
    val domain = data.get(from).getOrElse(throw new actions.NoSuchDomainException)
    val drop = new SomeDrop(nextToken getOrElse 0)
    val (items, numOfItems, hasMore) = limit.limit(drop.drop(order.sort(domain.iterator.filter(where).toSeq)))
    (output.what(items), numOfItems, hasMore)
  }
}

sealed abstract class OutputClause {
  type OutputSeq = Seq[(String, Seq[(String, String)])]
  def what(items: Seq[Item]): OutputSeq

  protected def flatAttrs(attrs: Iterator[Attribute]): Seq[(String, String)] = {
    attrs.flatMap((a: Attribute) => a.iterator.map((v: String) => (a.name, v))).toSeq
  }
}

case class SomeOutput(attrNames: List[String]) extends OutputClause {
  def what(items: Seq[Item]): OutputSeq = {
    items.view.map((item: Item) =>
      (item.name, flatAttrs(item.iterator.filter((a: Attribute) => attrNames.contains(a.name))))
    ).filter(!_._2.isEmpty).force
  }
}

case object ItemsOutput extends OutputClause {
  def what(items: Seq[Item]): OutputSeq = {
    items map { (item: Item) => (item.name, Seq.empty) }
  }
}

case object AllOutput extends OutputClause {
  def what(items: Seq[Item]): OutputSeq = {
    items.view.map((item: Item) =>
      (item.name, flatAttrs(item.iterator))
    ).filter(!_._2.isEmpty).force
  }
}

case object CountOutput extends OutputClause {
  def what(items: Seq[Item]): OutputSeq =
    Seq(("Domain", Seq(("Count", items.size.toString))))
}

sealed abstract class AttributePredicate extends Function1[String, Boolean] {
  /** Lifted logical and */
  def &&(that: AttributePredicate)= new AttributePredicate {
    def apply(v: String): Boolean = AttributePredicate.this(v) && that(v)
  }
  /** Lifted logical or */
  def ||(that: AttributePredicate) = new AttributePredicate {
    def apply(v: String): Boolean = AttributePredicate.this(v) || that(v)
  }
}

case class BinOpPredicate(op: String, value: String) extends AttributePredicate {
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

case class RangePredicate(lower: String, upper: String) extends AttributePredicate {
  def apply(v: String): Boolean = (v >= lower) && (v <= upper)
}

case class ContainsPredicate(values: Set[String]) extends AttributePredicate {
  def apply(v: String): Boolean = values.contains(v)
}

sealed abstract class ItemPredicate extends Function1[Item, Boolean] {
  /** Lifted logical and */
  def &&(that: ItemPredicate): ItemPredicate = (this, that) match {
    case (ExistsPredicate(name1, pred1), ExistsPredicate(name2, pred2)) if (name1 == name2) =>
      new ExistsPredicate(name1, pred1 && pred2)
    case _ => new ItemPredicate {
      def apply(item: Item): Boolean = ItemPredicate.this(item) && that(item)
    }
  }
  /** Lifted logical or */
  def ||(that: ItemPredicate): ItemPredicate = (this, that) match {
    case (EveryPredicate(name1, pred1), EveryPredicate(name2, pred2)) if (name1 == name2) =>
      new EveryPredicate(name1, pred1 || pred2)
    case _ => new ItemPredicate {
      def apply(item: Item): Boolean = ItemPredicate.this(item) || that(item)
    }
  }
}

case object NoopItemPredicate extends ItemPredicate {
  def apply(item: Item): Boolean = true
}

case class ExistsPredicate(attrName: String, pred: AttributePredicate) extends ItemPredicate {
  val cachedPred =
    if (attrName == "itemName()")
      ((item: Item) => pred(item.name))
    else
      ((item: Item) => item.get(attrName) exists { attr => attr.iterator.exists(pred) })
  def apply(item: Item): Boolean = cachedPred(item)
}

case class EveryPredicate(attrName: String, pred: AttributePredicate) extends ItemPredicate {
  def apply(item: Item): Boolean = item.get(attrName) exists { attr => attr.iterator.forall(pred) }
}

case class IsNullItemPredicate(attrName: String, isNull: Boolean) extends ItemPredicate {
  def apply(item: Item): Boolean = isNull == item.get(attrName).isEmpty
}

case class CompoundPredicate(pred1: ItemPredicate, op: String, pred2: ItemPredicate) extends ItemPredicate {
  protected def combine(): ItemPredicate = {
    val pred3 = pred1 match {
      case cp: CompoundPredicate => cp.combine()
      case _ => pred1
    }
    val pred4 = pred2 match {
      case cp: CompoundPredicate => cp.combine()
      case _ => pred2
    }
    op match {
      case "and" => pred3 && pred4
      case "or"  => pred3 || pred4
      case "intersection" => new ItemPredicate {
        def apply(item: Item) = pred3(item) && pred4(item)
      }
    }
  }
  def apply(item: Item): Boolean = combine()(item)
}

sealed abstract class LimitClause {
  def limit(items: Seq[Item]): (Seq[Item], Int, Boolean)
}

case object NoopLimit extends LimitClause {
  def limit(items: Seq[Item]) = (items, items.length, false)
}

case class LimitBy(limit: Int) extends LimitClause {
  def limit(items: Seq[Item]) = {
    val l = items.length
    (items take limit, l, l > limit)
  }
}

case class SomeDrop(count: Int) {
  def drop(items: Seq[Item]) = items drop count
}

sealed abstract class OrderClause {
  def sort(items: Seq[Item]): Seq[Item]
}

case object NoopOrder extends OrderClause {
  def sort(items: Seq[Item]) = items
}

case class OrderBy(attrName: String, way: String) extends OrderClause {
  def sort(items: Seq[Item]): Seq[Item] = {
    val comp = (lv: String, rv: String) => way match {
      case "desc" => lv > rv
      case _ => lv < rv
    }
    items.sortWith((l, r) => {
      comp(resolveValue(l), resolveValue(r))
    })
  }
  def resolveValue(item: Item) = {
    if (attrName == "itemName()") {
      item.name
    } else {
      item.get(attrName).map(attr => attr.iterator.next).getOrElse("")
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
         ^^ { case ol ~ i ~ w ~ o ~ l => SelectQuery(ol, i, w, o, l) }
    )

  def order: Parser[OrderClause] =
    ( "order" ~> "by" ~> ident ~! opt("asc" | "desc")
        ^^ { case i ~ way => OrderBy(i, way.getOrElse("asc")) }
    | success(NoopOrder)
    )

  def limit: Parser[LimitClause] =
    ( "limit" ~> numericLit ^^ { num => LimitBy(num.toInt) }
    | success(NoopLimit)
    )

  def whereClause: Parser[ItemPredicate] =
    ( "where" ~> where
    | success(NoopItemPredicate)
    )

  def setOp = "and" | "or" | "intersection"

  def where: Parser[ItemPredicate] =
    ( simplePredicate ~! opt(setOp ~! where)
        ^^ { case sp ~ opt_rest => opt_rest match {
               case Some(op ~ rp) => CompoundPredicate(sp, op, rp)
               case None          => sp } } )

  def op = "=" | "!=" | ">" | "<" | ">=" | "<=" | "like" | "not" ~ "like" ^^^ { "not-like" }

  def simplePredicate: Parser[ItemPredicate] =
    ( "(" ~> where <~ ")"
    | ("every" ~> "(" ~> ident <~ ")") ~! comparisonOp
        ^^ { case i ~ p => EveryPredicate(i, p)}
    | ident ~! ("is" ~> ( "null"          ^^^ { IsNullItemPredicate(_: String, true) }
                        | "not" ~! "null" ^^^ { IsNullItemPredicate(_: String, false) }
                        )
               | comparisonOp ^^ { pred => ExistsPredicate(_:String, pred) }
               ) ^^ { case i ~ f => f(i)}
    )

  def comparisonOp: Parser[AttributePredicate] =
    ( ("between" ~> stringLit) ~! ("and" ~> stringLit)
        ^^ { case lower ~ upper => RangePredicate(lower, upper) }
    | ("in" ~> "(" ~> repsep(stringLit, ",") <~ ")")
        ^^ { strs => ContainsPredicate(strs.toSet) }
    | op ~! stringLit
        ^^ { case o ~ v => BinOpPredicate(o, v) }
    )

  def outputList: Parser[OutputClause] =
    ( "*"                 ^^^ { AllOutput }
    | "count(*)"          ^^^ { CountOutput }
    | rep1sep(ident, ",")
        ^^  { _ match {
                case List("itemName()") => ItemsOutput
                case attrs => SomeOutput(attrs) } }
    )

  def makeSelectEval(input: String): SelectQuery = {
    val tokens = new lexical.Scanner(input)
    phrase(expr)(tokens) match {
      case Success(selectEval, _) => selectEval
      case Failure(msg, _) => sys.error(msg)
      case Error(msg, _) => sys.error(msg)
    }
  }
}
