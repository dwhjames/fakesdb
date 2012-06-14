package fakesdb

import fakesdb.actions.{DeleteAttributes, SDBException, MissingDomainNameException, MissingItemNameException}
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class DeleteAttributesSpec extends FlatSpec with ShouldMatchers {

  "The DeleteAttributes action" should "throw an exception for missing DomainName param" in {
    evaluating { new DeleteAttributes(new Data).handle(Map.empty) } should produce [MissingDomainNameException]
  }

  it should "throw an exception for missing ItemName param" in {
    val data = new Data
    data.getOrCreateDomain("abc")
    evaluating { new DeleteAttributes(data).handle(Map("DomainName" -> "abc")) } should produce [MissingItemNameException]
  }
}
