package fakesdb

import fakesdb.actions._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class CreateDomainSpec extends FlatSpec with ShouldMatchers {

  val data = new Data

  "The CreateDomain action" should "throw an exception for missing DomainName param" in {
    evaluating { new CreateDomain(data).handle(Map.empty) } should produce [MissingDomainNameException]
  }

  it should "throw an exception for an invalid domain name" in {
    var thrown = evaluating { new CreateDomain(data).handle(Map("DomainName" -> "ab"))} should produce [InvalidParameterValue]
    thrown.message should equal ("Value (ab) for parameter DomainName is invalid.")
    thrown = evaluating { new CreateDomain(data).handle(Map("DomainName" -> "abc!"))} should produce [InvalidParameterValue]
    thrown.message should equal ("Value (abc!) for parameter DomainName is invalid.")
  }

  it should "create a new domain" in {
    data.size should be (0)
    new CreateDomain(data).handle(Map("DomainName" -> "mydom"))
    data.size should be (1)
    data.get("mydom").get
    data.flush()
  }

  it should "ensure domain creation is idempotent" in {
    data.size should be (0)
    val domain = data.getOrCreate("mydom")
    new CreateDomain(data).handle(Map("DomainName" -> "mydom"))
    data.size should be (1)
    data.get("mydom").get should be (domain)
    data.flush()
  }

  it should "throw an exception when the domain limit has been exceeded" in {
    val d = new Data
    for (i <- 1 to Limits.MaxNumOfDomains) d.getOrCreate("abc"+i)
    val thrown = evaluating { new CreateDomain(d).handle(Map("DomainName" -> "abc%s".format(Limits.MaxNumOfDomains + 1)))} should produce [SDBException]
    thrown.xmlCode should equal ("NumberDomainsExceeded")
  }
}
