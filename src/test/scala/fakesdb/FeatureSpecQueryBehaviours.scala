package fakesdb

import org.scalatest.FeatureSpec
import org.scalatest.matchers.ShouldMatchers

trait FeatureSpecQueryBehaviors { this: FeatureSpec with ShouldMatchers =>

  def simpleQueries(queryItemNames: (String) => Seq[String]) {

    scenario("a single equality condition") {
      val items = queryItemNames("select * from mydomain where Title = 'The Right Stuff'")
      items should have size (1)
      items should contain ("1579124585")
    }

    scenario("a single greater-than condition") {
      val items = queryItemNames("select * from mydomain where Year > '1985'")
      items should have size (3)
      items should contain ("B000T9886K")
      items should contain ("B00005JPLW")
      items should contain ("B000SF3NGK")
    }

    scenario("a single like condition") {
      val items = queryItemNames("select * from mydomain where Rating like '****%'")
      items should have size (4)
      items should contain ("0385333498")
      items should contain ("0802131786")
      items should contain ("1579124585")
      items should contain ("B000SF3NGK")
    }

    scenario("a single less-than condition") {
      val items = queryItemNames("select * from mydomain where Pages < '00320'")
      items should have size (2)
      items should contain ("0802131786")
      items should contain ("1579124585")
    }
  }

  def rangeQueries(queryItemNames: (String) => Seq[String]) {

    scenario("a conjunction of inequalities") {
      val items = queryItemNames("select * from mydomain where Year > '1975' and Year < '2008'")
      items should have size (4)
      items should contain ("1579124585")
      items should contain ("B000T9886K")
      items should contain ("B00005JPLW")
      items should contain ("B000SF3NGK")
    }

    scenario("a range predicate") {
      val items = queryItemNames("select * from mydomain where Year between '1975' and '2008'")
      items should have size (4)
      items should contain ("1579124585")
      items should contain ("B000T9886K")
      items should contain ("B00005JPLW")
      items should contain ("B000SF3NGK")
    }

    scenario("a disjunction of equalities") {
      val items = queryItemNames("select * from mydomain where Rating = '***' or Rating = '*****'")
      items should have size (3)
      items should contain ("0385333498")
      items should contain ("B00005JPLW")
      items should contain ("B000SF3NGK")
    }

    scenario("a disjunction of conjunctions") {
      val items = queryItemNames("select * from mydomain where (Year > '1950' and Year < '1960') or Year like '193%' or Year = '2007'")
      items should have size (4)
      items should contain ("0385333498")
      items should contain ("0802131786")
      items should contain ("B000T9886K")
      items should contain ("B00005JPLW")
    }
  }

  def multiValuedAttributeQueries(queryItemNames: (String) => Seq[String]) {

    scenario("a disjunction of equalities") {
      val items = queryItemNames("select * from mydomain where Rating = '4 stars' or Rating = '****'")
      items should have size (3)
      items should contain ("0802131786")
      items should contain ("1579124585")
      items should contain ("B000T9886K")
    }

    scenario("a conjunction of equalities") {
      val items = queryItemNames("select * from mydomain where Keyword = 'Book' and Keyword = 'Hardcover'")
      items should have size (0)
    }

    scenario("a forall membership predicate") {
      val items = queryItemNames("select * from mydomain where every(Keyword) in ('Book', 'Paperback')")
      items should have size (2)
      items should contain ("0385333498")
      items should contain ("0802131786")
    }
  }

  def multiAttributeQueries(queryItemNames: (String) => Seq[String]) {

    scenario("a single equality condition") {
      val items = queryItemNames("select * from mydomain where Rating = '****'")
      items should have size (2)
      items should contain ("0802131786")
      items should contain ("1579124585")
    }

    scenario("a forall equality condition") {
      val items = queryItemNames("select * from mydomain where every(Rating) = '****'")
      items should have size (1)
      items should contain ("0802131786")
    }

    scenario("an intersection of equalities") {
      val items = queryItemNames("select * from mydomain where Keyword = 'Book' intersection Keyword = 'Hardcover'")
      items should have size (1)
      items should contain ("1579124585")
    }
  }

  def sortQueries(queryItemNames: (String) => Seq[String]) {

    scenario("explicitly order ascending") {
      val items = queryItemNames("select * from mydomain where Year < '1980' order by Year asc")
      items should have size (3)
      items(0) should equal ("0802131786")
      items(1) should equal ("0385333498")
      items(2) should equal ("1579124585")
    }

    scenario("use default ascending order") {
      val items = queryItemNames("select * from mydomain where Year < '1980' order by Year")
      items should have size (3)
      items(0) should equal ("0802131786")
      items(1) should equal ("0385333498")
      items(2) should equal ("1579124585")
    }

    scenario("explicitly order descending") {
      val items = queryItemNames("select * from mydomain where Year = '2007' intersection Author is not null order by Author desc")
      items should have size (2)
      items(0) should equal ("B00005JPLW")
      items(1) should equal ("B000T9886K")
    }

    ignore("an ordering clause unconstrained by a where clause") {
      evaluating {
        queryItemNames("select * from mydomain order by Year asc")
      } should produce [Exception]
    }

    scenario("order with a limit") {
      val items = queryItemNames("select * from mydomain where Year < '1980' order by Year limit 2")
      items should have size (2)
      items(0) should equal ("0802131786")
      items(1) should equal ("0385333498")
    }

    scenario("order by itemName()") {
      val items = queryItemNames("select itemName() from mydomain where itemName() like 'B000%' order by itemName()")
      items should have size (3)
      items(0) should equal ("B00005JPLW")
      items(1) should equal ("B000SF3NGK")
      items(2) should equal ("B000T9886K")
    }
  }

  def countQueries(queryCount: (String) => Int) {

    scenario("count with simple equality condition") {
      val count = queryCount("select count(*) from mydomain where Title = 'The Right Stuff'")
      count should be (1)
    }

    scenario("count with simple inequality condition") {
      val count = queryCount("select count(*) from mydomain where Year > '1985'")
      count should be (3)
    }

    scenario("count with large limit") {
      val count = queryCount("select count(*) from mydomain limit 500")
      count should be (6)
    }

    scenario("count with small limit") {
      val count = queryCount("select count(*) from mydomain limit 4")
      count should be (4)
    }
  }
}
