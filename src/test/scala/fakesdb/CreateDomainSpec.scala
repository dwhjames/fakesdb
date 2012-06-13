package fakesdb

import fakesdb.actions.{CreateDomain, SDBException, InvalidParameterValue, MissingDomainNameException}
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable.LinkedHashMap

class CreateDomainSpec extends FlatSpec with ShouldMatchers {

  val data = new Data

  "The CreateDomain action" should "throw an exception for missing DomainName param" in {
    evaluating { new CreateDomain(data).handle(new Params) } should produce [MissingDomainNameException]
  }

  it should "throw an exception for an invalid domain name" in {
    var thrown = evaluating { new CreateDomain(data).handle(new Params += ("DomainName" -> "ab"))} should produce [InvalidParameterValue]
    thrown.message should equal ("Value (ab) for parameter DomainName is invalid.")
    thrown = evaluating { new CreateDomain(data).handle(new Params += ("DomainName" -> "abc!"))} should produce [InvalidParameterValue]
    thrown.message should equal ("Value (abc!) for parameter DomainName is invalid.")
  }

  it should "throw an exception when the domain limit has been exceeded" in {
    val d = new Data
    for (i <- 1 to 250) {
      d.getOrCreateDomain("abc"+i)
    }
    val thrown = evaluating { new CreateDomain(d).handle(new Params += ("DomainName" -> "abc251"))} should produce [SDBException]
    thrown.httpStatus should equal (409)
    thrown.xmlCode should equal ("NumberDomainsExceeded")
    thrown.message should equal ("The domain limit was exceeded.")
  }
}
