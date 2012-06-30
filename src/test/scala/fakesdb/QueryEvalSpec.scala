package fakesdb

import org.scalatest.FeatureSpec
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.ShouldMatchers

class QueryEvalSpec extends FeatureSpec with BeforeAndAfter with ShouldMatchers with FeatureSpecQueryBehaviors {

  val data = new Data

  before {
    data.flush()
    val domain = data.getOrCreate(TestData.domainName)

    for ((itemName, attrs) <- TestData.items) {
      val item = domain.getOrCreate(itemName)
      for ((name, value) <- attrs) item.put(name, value, false)
    }
  }

  private def queryItemNames(query: String): Seq[String] =
    SelectParser.makeSelectEval(query).select(data)._1.map(_._1)

  private def queryCount(query: String): Int = {
    val res = SelectParser.makeSelectEval(query).select(data)._1
    res should have size (1)
    res(0)._1 should equal ("Domain")
    val attrs = res(0)._2
    attrs(0)._1 should equal ("Count")
    attrs(0)._2.toInt
  }

  feature("Simple Queries") {
    scenariosFor(simpleQueries(queryItemNames))
  }

  feature("Range Queries") {
    scenariosFor(rangeQueries(queryItemNames))
  }

  feature("Queries on Attributes with Multiple Values") {
    scenariosFor(multiValuedAttributeQueries(queryItemNames))
  }

  feature("Multiple Attribute Queries") {
    scenariosFor(multiAttributeQueries(queryItemNames))
  }

  feature("Sort Queries") {
    scenariosFor(sortQueries(queryItemNames))
  }

  feature("Count Queries") {
    scenariosFor(countQueries(queryCount))
  }

  feature("Limit Queries") {
    scenario("ordered ascending and limited") {
      val query = "select itemName() from mydomain where Year < '1980' order by Year asc limit 1"
      var xmlResp = new actions.Select(data).handle(Map("SelectExpression" -> query))
      var items = (xmlResp \\ "Item" \ "Name").map(_.text)
      items should have size (1)
      items(0) should equal ("0802131786")
      var token = (xmlResp \\ "NextToken")
      token should not be ('empty)

      xmlResp = new actions.Select(data).handle(Map("SelectExpression" -> query, "NextToken" -> token.text))
      items = (xmlResp \\ "Item" \ "Name").map(_.text)
      items should have size (1)
      items(0) should equal ("0385333498")
      token = (xmlResp \\ "NextToken")
      token should not be ('empty)

      xmlResp = new actions.Select(data).handle(Map("SelectExpression" -> query, "NextToken" -> token.text))
      items = (xmlResp \\ "Item" \ "Name").map(_.text)
      items should have size (1)
      items(0) should equal ("1579124585")
      token = (xmlResp \\ "NextToken")
      token should be ('empty)
    }
  }
}
