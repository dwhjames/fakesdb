package fakesdb

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

class QueryEvalSuite extends FunSuite {
  
  val data = new Data
  
  {
    val domain = data.getOrCreateDomain("mydomain")
    
    domain.getOrCreateItem("0385333498").put("Title", "The Sirens of Titan", false)
    domain.getOrCreateItem("0385333498").put("Author", "Kurt Vonnegut", false)
    domain.getOrCreateItem("0385333498").put("Year", "1959", false)
    domain.getOrCreateItem("0385333498").put("Pages", "00336", false)
    domain.getOrCreateItem("0385333498").put("Keyword", Seq("Book", "Paperback"), false)
    domain.getOrCreateItem("0385333498").put("Rating", Seq("*****", "5 stars", "Excellent"), false)
    
    domain.getOrCreateItem("0802131786").put("Title", "Tropic of Cancer", false)
    domain.getOrCreateItem("0802131786").put("Author", "Henry Miller", false)
    domain.getOrCreateItem("0802131786").put("Year", "1934", false)
    domain.getOrCreateItem("0802131786").put("Pages", "00318", false)
    domain.getOrCreateItem("0802131786").put("Keyword", "Book", false)
    domain.getOrCreateItem("0802131786").put("Rating", "****", false)
    
    domain.getOrCreateItem("1579124585").put("Title", "The Right Stuff", false)
    domain.getOrCreateItem("1579124585").put("Author", "Tom Wolfe", false)
    domain.getOrCreateItem("1579124585").put("Year", "1979", false)
    domain.getOrCreateItem("1579124585").put("Pages", "00304", false)
    domain.getOrCreateItem("1579124585").put("Keyword", Seq("Book", "Hardcover", "American"), false)
    domain.getOrCreateItem("1579124585").put("Rating", Seq("****", "4 stars"), false)
    
    domain.getOrCreateItem("B000T9886K").put("Title", "In Between", false)
    domain.getOrCreateItem("B000T9886K").put("Author", "Paul Van Dyk", false)
    domain.getOrCreateItem("B000T9886K").put("Year", "2007", false)
    domain.getOrCreateItem("B000T9886K").put("Keyword", Seq("CD", "Trance"), false)
    domain.getOrCreateItem("B000T9886K").put("Rating", "4 stars", false)
    
    domain.getOrCreateItem("B00005JPLW").put("Title", "300", false)
    domain.getOrCreateItem("B00005JPLW").put("Author", "Zack Snyder", false)
    domain.getOrCreateItem("B00005JPLW").put("Year", "2007", false)
    domain.getOrCreateItem("B00005JPLW").put("Keyword", Seq("DVD", "Action", "Frank Miller"), false)
    domain.getOrCreateItem("B00005JPLW").put("Rating", Seq("***", "3 stars", "Not bad"), false)
    
    domain.getOrCreateItem("B000SF3NGK").put("Title", "Heaven's Gonna Burn Your Eyes", false)
    domain.getOrCreateItem("B000SF3NGK").put("Author", "Thievery Corporation", false)
    domain.getOrCreateItem("B000SF3NGK").put("Year", "2002", false)
    domain.getOrCreateItem("B000SF3NGK").put("Rating", "*****", false)
  }
  
  private def queryItemNames(query: String): Seq[String] =
    SelectParser.makeSelectEval(query).select(data)._1.map(_._1)
  
  test("Simple Queries") {
    var items = queryItemNames("select * from mydomain where Title = 'The Right Stuff'")
    assert(items.size === 1)
    assert(items === Seq("1579124585"))
    
    items = queryItemNames("select * from mydomain where Year > '1985'")
    assert(items.size === 3)
    assert(items === Seq("B000T9886K", "B00005JPLW", "B000SF3NGK"))
    
    items = queryItemNames("select * from mydomain where Rating like '****%'")
    assert(items.size === 4)
    assert(items === Seq("0385333498", "0802131786", "1579124585", "B000SF3NGK"))
    
    items = queryItemNames("select * from mydomain where Pages < '00320'")
    assert(items.size === 2)
    assert(items === Seq("0802131786", "1579124585"))
  }
  
  test("Range Queries") {
    var items = queryItemNames("select * from mydomain where Year > '1975' and Year < '2008'")
    assert(items.size === 4)
    assert(items === Seq("1579124585", "B000T9886K", "B00005JPLW", "B000SF3NGK"))
    
    items = queryItemNames("select * from mydomain where Year between '1975' and '2008'")
    assert(items.size === 4)
    assert(items === Seq("1579124585", "B000T9886K", "B00005JPLW", "B000SF3NGK"))
    
    items = queryItemNames("select * from mydomain where Rating = '***' or Rating = '*****'")
    assert(items.size === 3)
    assert(items === Seq("0385333498", "B00005JPLW", "B000SF3NGK"))
    
    items = queryItemNames("select * from mydomain where (Year > '1950' and Year < '1960') or Year like '193%' or Year = '2007'")
    assert(items.size === 4)
    assert(items === Seq("0385333498", "0802131786", "B000T9886K", "B00005JPLW"))
  }
  
  test("Queries on Attributes with Multiple Values") {
    
    var items = queryItemNames("select * from mydomain where Rating = '4 stars' or Rating = '****'")
    assert(items.size === 3)
    assert(items === Seq("0802131786", "1579124585", "B000T9886K"))
    
    items = queryItemNames("select * from mydomain where Keyword = 'Book' and Keyword = 'Hardcover'")
    assert(items.size === 0)
    
    items = queryItemNames("select * from mydomain where every(Keyword) in ('Book', 'Paperback')")
    assert(items.size === 2)
    assert(items === Seq("0385333498", "0802131786"))
  }
  
  test("Multiple Attribute Queries") {
    var items = queryItemNames("select * from mydomain where Rating = '****'")
    assert(items.size === 2)
    assert(items === Seq("0802131786", "1579124585"))
    
    items = queryItemNames("select * from mydomain where every(Rating) = '****'")
    assert(items.size === 1)
    assert(items === Seq("0802131786"))
    
    items = queryItemNames("select * from mydomain where Keyword = 'Book' intersection Keyword = 'Hardcover'")
    assert(items.size === 1)
    assert(items === Seq("1579124585"))
  }
  
  test("Sort Queries") {
    var items = queryItemNames("select * from mydomain where Year < '1980' order by Year asc")
    assert(items.size === 3)
    assert(items === Seq("0802131786", "0385333498", "1579124585"))
    
    items = queryItemNames("select * from mydomain where Year < '1980' order by Year")
    assert(items.size === 3)
    assert(items === Seq("0802131786", "0385333498", "1579124585"))
    
    items = queryItemNames("select * from mydomain where Year = '2007' intersection Author is not null order by Author desc")
    assert(items.size === 2)
    assert(items === Seq("B00005JPLW", "B000T9886K"))
    
    /* bug: select query not validated
    intercept[Exception] {
      // Invalid because Year is not constrained by a predicate in the where clause.
      queryItemNames("select * from mydomain order by Year asc")
    }
    */
    
    items = queryItemNames("select * from mydomain where Year < '1980' order by Year limit 2")
    assert(items.size === 2)
    assert(items === Seq("0802131786", "0385333498"))
    
    items = queryItemNames("select itemName() from mydomain where itemName() like 'B000%' order by itemName()")
    assert(items.size === 3)
    assert(items === Seq("B00005JPLW", "B000SF3NGK", "B000T9886K"))
  }
}
