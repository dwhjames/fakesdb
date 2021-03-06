package fakesdb

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class QueryParserSpec extends FlatSpec with ShouldMatchers {

  val syntactic = SelectParser
  import SelectParser._

  private def parsing[T](s:String)(implicit p:Parser[T]):T = {
    syntactic.phrase(p)(new syntactic.lexical.Scanner(s)) match {
        case Success(t,_)     => t
        case NoSuccess(msg,_) =>
          throw new Exception("Could not parse '" + s + "': " + msg)
    }
  }

  "The Query Parser" should "parse identifiers" in {
    implicit val parserToTest = ident
    parsing("a")   should equal ("a")
    parsing("_")   should equal ("_")
    parsing("$")   should equal ("$")
    parsing("a1")  should equal ("a1")
    parsing("a_b") should equal ("a_b")
    parsing("a$b") should equal ("a$b")
    parsing("a1b") should equal ("a1b")
    
    parsing("`a`") should equal ("a")
    parsing("`a-b.c?`") should equal ("a-b.c?")
    parsing("`a``b`") should equal ("a`b")
    
    parsing("itemName()") should equal ("itemName()")
    
    evaluating { parsing("a-b") } should produce [Exception]
    evaluating { parsing("a.b") } should produce [Exception]
  }

  it should "parse string literals" in {
    implicit val parserToTest = stringLit
    parsing("\"\"") should equal ("")
    parsing("\'\'") should equal ("")
    parsing("\"a\"") should equal ("a")
    parsing("\'a\'") should equal ("a")
    parsing("\"a\"\"b\"") should equal ("a\"b")
    parsing("\'a\'\'b\'") should equal ("a\'b")
    parsing("\"a\'b\"") should equal ("a\'b")
    parsing("\'a\"b\'") should equal ("a\"b")
    
    parsing("\'He said, \"That\'\'s the ticket!\"\'") should equal ("He said, \"That's the ticket!\"")
    parsing("\"He said, \"\"That's the ticket!\"\"\"") should equal ("He said, \"That's the ticket!\"")
    
    evaluating { parsing("\"a\'") } should produce [Exception]
    evaluating { parsing("\'a\"") } should produce [Exception]
    evaluating { parsing("\"a") }   should produce [Exception]
    evaluating { parsing("\'a") }   should produce [Exception]
  }

  it should "parse output lists" in {
    implicit val parserToTest = outputList
    parsing("*") should equal (AllOutput)
    parsing("itemName()") should equal (ItemsOutput)
    parsing("count(*)") should equal (CountOutput)
    parsing("a") should equal (SomeOutput(List("a")))
    parsing("a, b") should equal (SomeOutput(List("a", "b")))
    parsing("a, b, c") should equal (SomeOutput(List("a", "b", "c")))
    
    evaluating { parsing("") } should produce [Exception]
    evaluating { parsing("a,") } should produce [Exception]
  }

  it should "parse limit clauses" in {
    implicit val parserToTest = limit
    parsing("") should equal (NoopLimit)
    parsing("limit 1")  should equal (LimitBy(1))
    parsing("limit 10") should equal (LimitBy(10))
    
    evaluating { parsing("limit -1") }  should produce [Exception]
    evaluating { parsing("limit 1.0") } should produce [Exception]
  }

  it should "parse order clauses" in {
    implicit val parserToTest = order
    parsing("") should equal (NoopOrder)
    parsing("order by a") should equal (OrderBy("a", "asc"))
    parsing("order by a asc") should equal (OrderBy("a", "asc"))
    parsing("order by a desc") should equal (OrderBy("a", "desc"))
    
    evaluating { parsing("order a") } should produce [Exception]
    evaluating { parsing("order by") } should produce [Exception]
    evaluating { parsing("order by asc") } should produce [Exception]
    evaluating { parsing("order by a up") } should produce [Exception]
  }

  it should "parse simple predicates" in {
    implicit val parserToTest = simplePredicate
    parsing("a =  'b'") should equal (ExistsPredicate("a", BinOpPredicate("=",  "b")))
    parsing("a != 'b'") should equal (ExistsPredicate("a", BinOpPredicate("!=", "b")))
    parsing("a >  'b'") should equal (ExistsPredicate("a", BinOpPredicate(">",  "b")))
    parsing("a >= 'b'") should equal (ExistsPredicate("a", BinOpPredicate(">=", "b")))
    parsing("a <  'b'") should equal (ExistsPredicate("a", BinOpPredicate("<",  "b")))
    parsing("a <= 'b'") should equal (ExistsPredicate("a", BinOpPredicate("<=", "b")))
    
    parsing("a like 'b'") should equal (ExistsPredicate("a", BinOpPredicate("like", "b")))
    parsing("a not like 'b'") should equal (ExistsPredicate("a", BinOpPredicate("not-like", "b")))
    evaluating { parsing("a not 'b'")} should produce [Exception]
    
    parsing("a is null") should equal (IsNullItemPredicate("a", true))
    evaluating { parsing("a null") } should produce [Exception]
    parsing("a is not null") should equal (IsNullItemPredicate("a", false))
    evaluating { parsing("a is not") } should produce [Exception]
    
    parsing("a between 'b' and 'c'") should equal (ExistsPredicate("a", RangePredicate("b", "c")))
    evaluating { parsing("a between 'b'") } should produce [Exception]
    evaluating { parsing("a between 'b' 'c'") } should produce [Exception]
    evaluating { parsing("a between 'b' and") } should produce [Exception]
    
    parsing("a in ('b')") should equal (ExistsPredicate("a", ContainsPredicate(Set("b"))))
    parsing("a in ('b', 'c')") should equal (ExistsPredicate("a", ContainsPredicate(Set("b", "c"))))
    evaluating { parsing("a in ('b',)")} should produce [Exception]
    evaluating { parsing("a in 'b'")} should produce [Exception]
    evaluating { parsing("a in 'b', 'c'")} should produce [Exception]
    
    parsing("every(a) = 'b'") should equal (EveryPredicate("a", BinOpPredicate("=", "b")))
    evaluating { parsing("every a = 'b'") } should produce [Exception]
    
    parsing("(a = 'b' and c = 'd')") should equal (CompoundPredicate(ExistsPredicate("a", BinOpPredicate("=",  "b")),
                                                   "and",
                                                   ExistsPredicate("c", BinOpPredicate("=",  "d"))))
  }

  it should "parse compound predicates" in {
    implicit val parserToTest = where
    parsing("a = 'b'") should equal (ExistsPredicate("a", BinOpPredicate("=", "b")))
    parsing("a = 'b' and c = 'd'") should equal (CompoundPredicate(ExistsPredicate("a", BinOpPredicate("=",  "b")),
                                                                   "and",
                                                                   ExistsPredicate("c", BinOpPredicate("=",  "d"))))
    
    parsing("a = 'b' or c = 'd'") should equal (CompoundPredicate(ExistsPredicate("a", BinOpPredicate("=",  "b")),
                                                                  "or",
                                                                  ExistsPredicate("c", BinOpPredicate("=",  "d"))))
    
    parsing("a = 'b' intersection c = 'd'") should equal (CompoundPredicate(ExistsPredicate("a", BinOpPredicate("=",  "b")),
                                                          "intersection",
                                                          ExistsPredicate("c", BinOpPredicate("=",  "d"))))
  }

  it should "parse where clauses" in {
    implicit val parserToTest = whereClause
    parsing("") should equal (NoopItemPredicate)
    parsing("where a = 'b'") should equal (ExistsPredicate("a", BinOpPredicate("=",  "b")))
  }

  it should "parse Amazon's examples" in {
    implicit val parserToTest = expr
    /* http://docs.amazonwebservices.com/AmazonSimpleDB/latest/DeveloperGuide/SimpleQueriesSelect.html */
    (
      parsing("select * from mydomain where Title = 'The Right Stuff'")
        should equal
          (SelectQuery(AllOutput, "mydomain",
                       ExistsPredicate("Title", BinOpPredicate("=", "The Right Stuff")),
                       NoopOrder, NoopLimit))
    )
    (
      parsing("select * from mydomain where Year > '1985'")
        should equal
          (SelectQuery(AllOutput, "mydomain",
                       ExistsPredicate("Year", BinOpPredicate(">", "1985")),
                       NoopOrder, NoopLimit))
    )
    (
      parsing("select * from mydomain where Rating like '****%'")
        should equal
          (SelectQuery(AllOutput, "mydomain",
                       ExistsPredicate("Rating", BinOpPredicate("like", "****%")),
                       NoopOrder, NoopLimit))
    )
    (
      parsing("select * from mydomain where Pages < '00320'")
        should equal
          (SelectQuery(AllOutput, "mydomain",
                       ExistsPredicate("Pages", BinOpPredicate("<", "00320")),
                       NoopOrder, NoopLimit))
    )
    /* http://docs.amazonwebservices.com/AmazonSimpleDB/latest/DeveloperGuide/RangeQueriesSelect.html */
    (
      parsing("select * from mydomain where Year > '1975' and Year < '2008'")
        should equal
          (SelectQuery(AllOutput, "mydomain",
                       CompoundPredicate(ExistsPredicate("Year", BinOpPredicate(">", "1975")),
                                         "and",
                                         ExistsPredicate("Year", BinOpPredicate("<", "2008"))
                                        ),
                       NoopOrder, NoopLimit))
    )
    (
      parsing("select * from mydomain where Year between '1975' and '2008'")
        should equal
          (SelectQuery(AllOutput, "mydomain",
                       ExistsPredicate("Year", RangePredicate("1975", "2008")),
                       NoopOrder, NoopLimit))
    )
    (
      parsing("select * from mydomain where Rating = '***' or Rating = '*****'")
        should equal
          (SelectQuery(AllOutput, "mydomain",
                       CompoundPredicate(ExistsPredicate("Rating", BinOpPredicate("=", "***")),
                                         "or",
                                         ExistsPredicate("Rating", BinOpPredicate("=", "*****"))
                                        ),
                       NoopOrder, NoopLimit))
    )
    (
      parsing("select * from mydomain where (Year > '1950' and Year < '1960') or Year like '193%' or Year = '2007'")
        should equal
          (SelectQuery(AllOutput, "mydomain",
                       CompoundPredicate(CompoundPredicate(ExistsPredicate("Year", BinOpPredicate(">", "1950")),
                                                           "and",
                                                           ExistsPredicate("Year", BinOpPredicate("<", "1960"))),
                                         "or",
                                         CompoundPredicate(ExistsPredicate("Year", BinOpPredicate("like", "193%")),
                                                           "or",
                                                           ExistsPredicate("Year", BinOpPredicate("=", "2007")))),
                       NoopOrder, NoopLimit))
    )
    /* http://docs.amazonwebservices.com/AmazonSimpleDB/latest/DeveloperGuide/RangeValueQueriesSelect.html */
    (
      parsing("select * from mydomain where Rating = '4 stars' or Rating = '****'")
        should equal
          (SelectQuery(AllOutput, "mydomain",
                       CompoundPredicate(ExistsPredicate("Rating", BinOpPredicate("=", "4 stars")),
                                         "or",
                                         ExistsPredicate("Rating", BinOpPredicate("=", "****"))
                                        ),
                       NoopOrder, NoopLimit))
    )
    (
      parsing("select * from mydomain where Keyword = 'Book' and Keyword = 'Hardcover'")
        should equal
          (SelectQuery(AllOutput, "mydomain",
                       CompoundPredicate(ExistsPredicate("Keyword", BinOpPredicate("=", "Book")),
                                         "and",
                                         ExistsPredicate("Keyword", BinOpPredicate("=", "Hardcover"))
                                        ),
                       NoopOrder, NoopLimit))
    )
    (
      parsing("select * from mydomain where every(keyword) in ('Book', 'Paperback')")
        should equal
          (SelectQuery(AllOutput, "mydomain",
                       EveryPredicate("keyword", ContainsPredicate(Set("Book", "Paperback"))),
                       NoopOrder, NoopLimit))
    )
    /* http://docs.amazonwebservices.com/AmazonSimpleDB/latest/DeveloperGuide/MultipleAttributeQueriesSelect.html */
    (
      parsing("select * from mydomain where Rating = '****'")
        should equal
          (SelectQuery(AllOutput, "mydomain",
                       ExistsPredicate("Rating", BinOpPredicate("=", "****")),
                       NoopOrder, NoopLimit))
    )
    (
      parsing("select * from mydomain where every(Rating) = '****'")
        should equal
          (SelectQuery(AllOutput, "mydomain",
                       EveryPredicate("Rating", BinOpPredicate("=", "****")),
                       NoopOrder, NoopLimit))
    )
    (
      parsing("select * from mydomain where Keyword = 'Book' intersection Keyword = 'Hardcover'")
        should equal
          (SelectQuery(AllOutput, "mydomain",
                       CompoundPredicate(ExistsPredicate("Keyword", BinOpPredicate("=", "Book")),
                                         "intersection",
                                         ExistsPredicate("Keyword", BinOpPredicate("=", "Hardcover"))
                                        ),
                       NoopOrder, NoopLimit))
    )
    /* http://docs.amazonwebservices.com/AmazonSimpleDB/latest/DeveloperGuide/SortingDataSelect.html */
    (
      parsing("select * from mydomain where Year < '1980' order by Year asc")
        should equal
          (SelectQuery(AllOutput, "mydomain",
                       ExistsPredicate("Year", BinOpPredicate("<", "1980")),
                       OrderBy("Year", "asc"), NoopLimit))
    )
    (
      parsing("select * from mydomain where Year < '1980' order by Year")
        should equal
          (SelectQuery(AllOutput, "mydomain",
                       ExistsPredicate("Year", BinOpPredicate("<", "1980")),
                       OrderBy("Year", "asc"), NoopLimit))
    )
    (
      parsing("select * from mydomain where Year = '2007' intersection Author is not null order by Author desc")
        should equal
          (SelectQuery(AllOutput, "mydomain",
                       CompoundPredicate(ExistsPredicate("Year", BinOpPredicate("=", "2007")),
                                         "intersection",
                                         IsNullItemPredicate("Author", false)
                                        ),
                       OrderBy("Author", "desc"), NoopLimit))
    )
    (
      parsing("select * from mydomain where Year < '1980' order by Year limit 2")
        should equal
          (SelectQuery(AllOutput, "mydomain",
                       ExistsPredicate("Year", BinOpPredicate("<", "1980")),
                       OrderBy("Year", "asc"), LimitBy(2)))
    )
    (
      parsing("select itemName() from mydomain where itemName() like 'B000%' order by itemName()")
        should equal
          (SelectQuery(ItemsOutput, "mydomain",
                       ExistsPredicate("itemName()", BinOpPredicate("like", "B000%")),
                       OrderBy("itemName()", "asc"), NoopLimit))
    )
    /* http://docs.amazonwebservices.com/AmazonSimpleDB/latest/DeveloperGuide/CountingDataSelect.html */
    (
      parsing("select count(*) from mydomain where Title = 'The Right Stuff'")
        should equal
          (SelectQuery(CountOutput, "mydomain",
                       ExistsPredicate("Title", BinOpPredicate("=", "The Right Stuff")),
                       NoopOrder, NoopLimit))
    )
    (
      parsing("select count(*) from mydomain where Year > '1985'")
        should equal
          (SelectQuery(CountOutput, "mydomain",
                       ExistsPredicate("Year", BinOpPredicate(">", "1985")),
                       NoopOrder, NoopLimit))
    )
    (
      parsing("select count(*) from mydomain limit 500")
        should equal
          (SelectQuery(CountOutput, "mydomain",
                       NoopItemPredicate,
                       NoopOrder, LimitBy(500)))
    )
    (
      parsing("select count(*) from mydomain limit 4")
        should equal
          (SelectQuery(CountOutput, "mydomain",
                       NoopItemPredicate,
                       NoopOrder, LimitBy(4)))
    )
  }
}
