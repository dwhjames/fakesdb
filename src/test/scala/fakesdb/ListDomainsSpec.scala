package fakesdb

import fakesdb.actions.{ListDomains, SDBException, InvalidNextTokenException, InvalidParameterValue}
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ListDomainsSpec extends FlatSpec with ShouldMatchers {

  "The ListDomains action" should "throw an exception for an invalid MaxNumberOfDomains param" in {
    val data = new Data

    var thrown = evaluating { new ListDomains(data).handle(Map("MaxNumberOfDomains" -> "foo")) } should produce [InvalidParameterValue]
    thrown.message should equal ("Value (foo) for parameter MaxNumberOfDomains is invalid. MaxNumberOfDomains must be between 1 and 100.")

    thrown = evaluating { new ListDomains(data).handle(Map("MaxNumberOfDomains" -> "-1")) } should produce [InvalidParameterValue]
    thrown.message should equal ("Value (-1) for parameter MaxNumberOfDomains is invalid. MaxNumberOfDomains must be between 1 and 100.")

    thrown = evaluating { new ListDomains(data).handle(Map("MaxNumberOfDomains" -> "0")) } should produce [InvalidParameterValue]
    thrown.message should equal ("Value (0) for parameter MaxNumberOfDomains is invalid. MaxNumberOfDomains must be between 1 and 100.")

    thrown = evaluating { new ListDomains(data).handle(Map("MaxNumberOfDomains" -> "101")) } should produce [InvalidParameterValue]
    thrown.message should equal ("Value (101) for parameter MaxNumberOfDomains is invalid. MaxNumberOfDomains must be between 1 and 100.")
  }

  it should "throw an exception for an invalid NextToken param" in {
    val data = new Data

    evaluating { new ListDomains(data).handle(Map("NextToken" -> "foo")) } should produce [InvalidNextTokenException]
    evaluating { new ListDomains(data).handle(Map("NextToken" -> "-1")) } should produce [InvalidNextTokenException]
    evaluating { new ListDomains(data).handle(Map("NextToken" -> "1")) } should produce [InvalidNextTokenException]
  }

  private def fixture(): Data = {
    val data = new Data
    data.getOrCreateDomain("abc1")
    data.getOrCreateDomain("abc2")
    data.getOrCreateDomain("abc3")
    data.getOrCreateDomain("abc4")
    data.getOrCreateDomain("abc5")
    data
  }

  it should "support the MaxNumberOfDomains param" in {
    val data = fixture()

    val xmlResp = new ListDomains(data).handle(Map("MaxNumberOfDomains" -> "3"))
    val domainNodes = xmlResp \\ "DomainName"
    domainNodes.size should equal (3)
    domainNodes(0).text should equal ("abc1")
    domainNodes(1).text should equal ("abc2")
    domainNodes(2).text should equal ("abc3")
    val tokenNode = xmlResp \\ "NextToken"
    tokenNode.size should equal (1)
    tokenNode(0).text should equal ("3")
  }

  it should "support the NextToken param" in {
    val data = fixture()

    val xmlResp = new ListDomains(data).handle(Map("NextToken" -> "3"))
    val domainNodes = xmlResp \\ "DomainName"
    domainNodes.size should equal (2)
    domainNodes(0).text should equal ("abc4")
    domainNodes(1).text should equal ("abc5")
    val tokenNode = xmlResp \\ "NextToken"
    tokenNode should be ('empty)
  }

  it should "support the MaxNumberOfDomains and NextToken param together" in {
    val data = fixture()

    val xmlResp = new ListDomains(data).handle(Map("MaxNumberOfDomains" -> "3", "NextToken" -> "1"))
    val domainNodes = xmlResp \\ "DomainName"
    domainNodes.size should equal (3)
    domainNodes(0).text should equal ("abc2")
    domainNodes(1).text should equal ("abc3")
    domainNodes(2).text should equal ("abc4")
    val tokenNode = xmlResp \\ "NextToken"
    tokenNode.size should equal (1)
    tokenNode(0).text should equal ("4")
  }
}
