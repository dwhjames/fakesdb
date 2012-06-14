package fakesdb

import fakesdb.actions.{GetAttributes, SDBException, MissingDomainNameException, MissingItemNameException}
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class GetAttributesSpec extends FlatSpec with ShouldMatchers {

  "The GetAttributes action" should "throw an exception for missing DomainName param" in {
    evaluating { new GetAttributes(new Data).handle(Map.empty) } should produce [MissingDomainNameException]
  }

  it should "throw an exception for missing ItemName param" in {
    val data = new Data
    data.getOrCreateDomain("abc")
    evaluating { new GetAttributes(data).handle(Map("DomainName" -> "abc")) } should produce [MissingItemNameException]
  }
}
