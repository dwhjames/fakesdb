package fakesdb

import fakesdb.actions.{PutAttributes, SDBException, MissingDomainNameException, MissingItemNameException}
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class PutAttributesSpec extends FlatSpec with ShouldMatchers {

  "The PutAttributes action" should "throw an exception for missing DomainName param" in {
    evaluating { new PutAttributes(new Data).handle(Map.empty) } should produce [MissingDomainNameException]
  }

  it should "throw an exception for missing ItemName param" in {
    val data = new Data
    data.getOrCreate("abc")
    evaluating { new PutAttributes(data).handle(Map("DomainName" -> "abc")) } should produce [MissingItemNameException]
  }
}
