package fakesdb

import fakesdb.actions.{GetAttributes, SDBException, MissingDomainNameException, MissingItemNameException, NoSuchDomainException}
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class GetAttributesSpec extends FlatSpec with ShouldMatchers {

  "The GetAttributes action" should "throw an exception for missing DomainName param" in {
    evaluating { new GetAttributes(new Data).handle(Map.empty) } should produce [MissingDomainNameException]
  }

  it should "throw an exception for a non-existant domain name" in {
    evaluating { new GetAttributes(new Data).handle(Map("DomainName" -> "abc"))} should produce [NoSuchDomainException]
  }

  it should "throw an exception for missing ItemName param" in {
    val data = new Data
    data.getOrCreate("abc")
    evaluating { new GetAttributes(data).handle(Map("DomainName" -> "abc")) } should produce [MissingItemNameException]
  }
}
