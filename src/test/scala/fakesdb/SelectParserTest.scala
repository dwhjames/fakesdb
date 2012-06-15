package fakesdb

import org.junit._
import org.junit.Assert._

class SelectParserTest {

  val data = new Data
  var domaina: Domain = null

  @Before
  def setUp(): Unit = {
    data.flush
    domaina = data.getOrCreate("domaina")
  }

  @Test
  def testEmptyFrom(): Unit = {
    val se = SelectParser.makeSelectEval("select * from domaina")
    assertEquals(0, se.select(data)._1.size)
  }

  @Test
  def testFromWithData(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itema").put("b", "2", true)
    domaina.getOrCreate("itemb").put("c", "3", true)
    val results = SelectParser.makeSelectEval("select * from domaina").select(data)._1
    assertEquals(2, results.size)
    assertEquals(("itema", List(("a", "1"), ("b", "2"))), results(0))
    assertEquals(("itemb", List(("c", "3"))), results(1))
  }
  
  @Test
  def testFromWithOneAttribute(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itema").put("b", "2", true)
    domaina.getOrCreate("itemb").put("a", "3", true)
    // a is in both
    val results = SelectParser.makeSelectEval("select a from domaina").select(data)._1
    assertEquals(2, results.size)
    assertEquals(("itema", List(("a", "1"))), results(0))
    assertEquals(("itemb", List(("a", "3"))), results(1))
    // b in in only one
    val results2 = SelectParser.makeSelectEval("select b from domaina").select(data)._1
    assertEquals(1, results2.size)
    assertEquals(("itema", List(("b", "2"))), results2(0))
  }

  @Test
  def testFromWithOneAttributeWithMultipleValues(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itema").put("a", "1", false)
    val results = SelectParser.makeSelectEval("select a from domaina").select(data)._1
    assertEquals(1, results.size)
    assertEquals(("itema", List(("a", "1"))), results(0))
  }

  @Test
  def testFromWithTwoAttributes(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itema").put("b", "2", true)
    domaina.getOrCreate("itemb").put("a", "3", true)
    val results = SelectParser.makeSelectEval("select a, b from domaina").select(data)._1
    assertEquals(2, results.size)
    assertEquals(("itema", List(("a", "1"), ("b", "2"))), results(0))
    assertEquals(("itemb", List(("a", "3"))), results(1))
  }

  @Test
  def testFromCount(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    val results = SelectParser.makeSelectEval("select count(*) from domaina").select(data)._1
    assertEquals(1, results.size)
    assertEquals(("Domain", List(("Count", "1"))), results(0))
  }

  @Test
  def testFromItemName(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    val results = SelectParser.makeSelectEval("select itemName() from domaina").select(data)._1
    assertEquals(1, results.size)
    assertEquals(("itema", List.empty), results(0))
  }

  @Test
  def testWhereEquals(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itemb").put("a", "2", true)
    val results = SelectParser.makeSelectEval("select * from domaina where a = '1'").select(data)._1
    assertEquals(1, results.size)
    assertEquals(("itema", List(("a", "1"))), results(0))
  }

  @Test
  def testWhereDoubleTicks(): Unit = {
    domaina.getOrCreate("itema").put("a", "1'1", true)
    domaina.getOrCreate("itemb").put("a", "2'2", true)
    val results = SelectParser.makeSelectEval("select * from domaina where a = '1''1'").select(data)._1
    assertEquals(1, results.size)
    assertEquals(("itema", List(("a", "1'1"))), results(0))
  }

  @Test
  def testWhereDoubleQuotes(): Unit = {
    domaina.getOrCreate("itema").put("a", "1\"1", true)
    domaina.getOrCreate("itemb").put("a", "2\"2", true)
    val results = SelectParser.makeSelectEval("select * from domaina where a = \"1\"\"1\"").select(data)._1
    assertEquals(1, results.size)
    assertEquals(("itema", List(("a", "1\"1"))), results(0))
  }

  @Test
  def testWhereNotEquals(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itemb").put("a", "2", true)
    val results = SelectParser.makeSelectEval("select * from domaina where a != '1'").select(data)._1
    assertEquals(1, results.size)
    assertEquals(("itemb", List(("a", "2"))), results(0))
  }

  @Test
  def testWhereGreaterThan(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itemb").put("a", "2", true)
    val results = SelectParser.makeSelectEval("select * from domaina where a > '1'").select(data)._1
    assertEquals(1, results.size)
    assertEquals(("itemb", List(("a", "2"))), results(0))
  }

  @Test
  def testWhereLessThan(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itemb").put("a", "2", true)
    val results = SelectParser.makeSelectEval("select * from domaina where a < '2'").select(data)._1
    assertEquals(1, results.size)
    assertEquals(("itema", List(("a", "1"))), results(0))
  }

  @Test
  def testWhereGreaterThanOrEqual(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itemb").put("a", "2", true)
    domaina.getOrCreate("itemc").put("a", "3", true)
    val results = SelectParser.makeSelectEval("select * from domaina where a >= '2'").select(data)._1
    assertEquals(2, results.size)
    assertEquals(("itemb", List(("a", "2"))), results(0))
    assertEquals(("itemc", List(("a", "3"))), results(1))
  }

  @Test
  def testWhereLessThanOrEqual(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itemb").put("a", "2", true)
    domaina.getOrCreate("itemc").put("a", "3", true)
    val results = SelectParser.makeSelectEval("select * from domaina where a <= '2'").select(data)._1
    assertEquals(2, results.size)
    assertEquals(("itema", List(("a", "1"))), results(0))
    assertEquals(("itemb", List(("a", "2"))), results(1))
  }

  @Test
  def testWhereLike(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itemb").put("a", "2", true)
    val results = SelectParser.makeSelectEval("select * from domaina where a like '1%'").select(data)._1
    assertEquals(1, results.size)
    assertEquals(("itema", List(("a", "1"))), results(0))
  }

  @Test
  def testWhereNotLike(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itemb").put("a", "2", true)
    val results = SelectParser.makeSelectEval("select * from domaina where a not like '1%'").select(data)._1
    assertEquals(1, results.size)
    assertEquals(("itemb", List(("a", "2"))), results(0))
  }

  @Test
  def testWhereIsNull(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itemb").put("b", "2", true)
    val results = SelectParser.makeSelectEval("select * from domaina where a is null").select(data)._1
    assertEquals(1, results.size)
    assertEquals(("itemb", List(("b", "2"))), results(0))
  }
  
  @Test
  def testWhereIsNotNull(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itemb").put("b", "2", true)
    val results = SelectParser.makeSelectEval("select * from domaina where a is not null").select(data)._1
    assertEquals(1, results.size)
    assertEquals(("itema", List(("a", "1"))), results(0))
  }

  @Test
  def testWhereBetween(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itemb").put("a", "2", true)
    val results = SelectParser.makeSelectEval("select * from domaina where a between '2' and '3'").select(data)._1
    assertEquals(1, results.size)
    assertEquals(("itemb", List(("a", "2"))), results(0))
  }

  @Test
  def testWhereEvery(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itemb").put("a", "1", true)
    domaina.getOrCreate("itemb").put("a", "2", true)
    val results = SelectParser.makeSelectEval("select * from domaina where every(a) = '1'").select(data)._1
    assertEquals(1, results.size)
    assertEquals(("itema", List(("a", "1"))), results(0))
  }

  @Test
  def testWhereIn(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itemb").put("a", "2", true)
    domaina.getOrCreate("itemc").put("a", "3", true)
    val results = SelectParser.makeSelectEval("select * from domaina where a in ('1', '2')").select(data)._1
    assertEquals(2, results.size)
    assertEquals(("itema", List(("a", "1"))), results(0))
    assertEquals(("itemb", List(("a", "2"))), results(1))
  }

  @Test
  def testWhereEqualsAnd(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itema").put("b", "2", true)
    domaina.getOrCreate("itemb").put("a", "1", true)
    val results = SelectParser.makeSelectEval("select * from domaina where a = '1' and b = '2'").select(data)._1
    assertEquals(1, results.size)
    assertEquals(("itema", List(("a", "1"), ("b", "2"))), results(0))
  }

  @Test
  def testWhereEqualsOr(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itemb").put("a", "2", true)
    val results = SelectParser.makeSelectEval("select * from domaina where a = '1' or a = '2'").select(data)._1
    assertEquals(2, results.size)
    assertEquals(("itema", List(("a", "1"))), results(0))
    assertEquals(("itemb", List(("a", "2"))), results(1))
  }

  @Test
  def testWhereParens(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itema").put("b", "1", true)
    domaina.getOrCreate("itemb").put("a", "2", true)
    val results = SelectParser.makeSelectEval("select * from domaina where (a = '1' and b = '1') or a = '2'").select(data)._1
    assertEquals(2, results.size)
    assertEquals(("itema", List(("a", "1"), ("b", "1"))), results(0))
    assertEquals(("itemb", List(("a", "2"))), results(1))
  }

  @Test
  def testOrderBy(): Unit = {
    domaina.getOrCreate("itema").put("a", "2", true)
    domaina.getOrCreate("itemb").put("a", "1", true)
    val results = SelectParser.makeSelectEval("select * from domaina where a >= '1' order by a").select(data)._1
    assertEquals(2, results.size)
    assertEquals(("itemb", List(("a", "1"))), results(0))
    assertEquals(("itema", List(("a", "2"))), results(1))
  }

  @Test
  def testOrderByDesc(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itemb").put("a", "2", true)
    val results = SelectParser.makeSelectEval("select * from domaina where a >= '1' order by a desc").select(data)._1
    assertEquals(2, results.size)
    assertEquals(("itemb", List(("a", "2"))), results(0))
    assertEquals(("itema", List(("a", "1"))), results(1))
  }

  @Test
  def testOrderByItemNameDesc(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itemb").put("a", "2", true)
    domaina.getOrCreate("itemc").put("a", "3", true)
    val results = SelectParser.makeSelectEval("select * from domaina where a >= '1' order by itemName() desc").select(data)._1
    assertEquals(3, results.size)
    assertEquals(("itemc", List(("a", "3"))), results(0))
    assertEquals(("itemb", List(("a", "2"))), results(1))
    assertEquals(("itema", List(("a", "1"))), results(2))
  }

  @Test
  def testWhereCrazyAnd(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itema").put("a", "2", false)
    val results = SelectParser.makeSelectEval("select * from domaina where a = '1' and a = '2'").select(data)._1
    assertEquals(0, results.size)
  }

  @Test
  def testWhereIntersection(): Unit = {
    domaina.getOrCreate("itema").put("a", "1", true)
    domaina.getOrCreate("itema").put("a", "2", false)
    domaina.getOrCreate("itemb").put("a", "1", true)
    val results = SelectParser.makeSelectEval("select * from domaina where a = '1' intersection a = '2'").select(data)._1
    assertEquals(1, results.size)
    assertEquals(("itema", List(("a", "1"), ("a", "2"))), results(0))
  }

  @Test
  def testKeyWithUnderscores(): Unit = {
    domaina.getOrCreate("itema").put("foo_bar", "1", true)
    val results = SelectParser.makeSelectEval("select * from domaina where foo_bar = '1'").select(data)._1
    assertEquals(1, results.size)
    assertEquals(("itema", List(("foo_bar", "1"))), results(0))
  }

  @Test
  def testLarrysQuery(): Unit = {
    val usage = data.getOrCreate("dev.api-web-usage")
    val a = usage.getOrCreate("itema")
    a.put("api_key", "1", true)
    a.put("dt", "2010010001", true)
    val results = SelectParser.makeSelectEval("select * from `dev.api-web-usage` where api_key = '1' and dt > '2010010000' and dt < '2010013224'").select(data)._1
    assertEquals(1, results.size)
  }

  @Test
  def testLimit(): Unit = {
    domaina.getOrCreate("itema").put("foo", "1", true)
    domaina.getOrCreate("itemb").put("foo", "2", true)
    domaina.getOrCreate("itemc").put("foo", "3", true)
    val results = SelectParser.makeSelectEval("select * from domaina where foo > '0' limit 2").select(data)._1
    assertEquals(2, results.size)
    assertEquals(("itema", List(("foo", "1"))), results(0))
    assertEquals(("itemb", List(("foo", "2"))), results(1))
    // Now with order by
    val results2 = SelectParser.makeSelectEval("select * from domaina where foo > '0' order by foo desc limit 2").select(data)._1
    assertEquals(2, results2.size)
    assertEquals(("itemc", List(("foo", "3"))), results2(0))
    assertEquals(("itemb", List(("foo", "2"))), results2(1))
  }

  @Test
  def testAttributeDoubleBacktick(): Unit = {
    domaina.getOrCreate("itema").put("a`a", "1", true)
    val results = SelectParser.makeSelectEval("select `a``a` from domaina where `a``a` = '1'").select(data)._1
    assertEquals(1, results.size)
    assertEquals(("itema", List(("a`a", "1"))), results(0))
  }

  @Test
  def testAttributeLegalChars(): Unit = {
    domaina.getOrCreate("itema").put("a1$_", "1", true)
    val results = SelectParser.makeSelectEval("select a1$_ from domaina where a1$_ = '1'").select(data)._1
    assertEquals(1, results.size)
    assertEquals(("itema", List(("a1$_", "1"))), results(0))
  }

  @Test
  def testAttributeLegalCharsInTheFirstPosition(): Unit = {
    domaina.getOrCreate("itema").put("$_", "1", true)
    val results = SelectParser.makeSelectEval("select $_ from domaina where $_ = '1'").select(data)._1
    assertEquals(1, results.size)
    assertEquals(("itema", List(("$_", "1"))), results(0))
  }
}
