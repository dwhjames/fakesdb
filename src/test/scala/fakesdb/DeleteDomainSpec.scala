package fakesdb

import fakesdb.actions._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class DeleteDomainSpec extends FlatSpec with ShouldMatchers {

  val data = new Data

  "The DeleteDomain action" should "throw an exception for missing DomainName param" in {
    evaluating { new DeleteDomain(data).handle(Map.empty) } should produce [MissingDomainNameException]
  }

  it should "ignore deletion of non-existant domain name" in {
    data.size should be (0)
    new DeleteDomain(data).handle(Map("DomainName" -> "mydom"))
    data.size should be (0)
  }

  it should "delete an existing domain" in {
    data.getOrCreate("mydom")
    data.size should be (1)
    new DeleteDomain(data).handle(Map("DomainName" -> "mydom"))
    data.size should be (0)
    data.flush()
  }
}
