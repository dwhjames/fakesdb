package fakesdb

import fakesdb.actions.{DeleteDomain, SDBException, MissingDomainNameException}
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class DeleteDomainSpec extends FlatSpec with ShouldMatchers {

  val data = new Data

  "The DeleteDomain action" should "throw an exception for missing DomainName param" in {
    evaluating { new DeleteDomain(data).handle(Map.empty) } should produce [MissingDomainNameException]
  }

  it should "throw an exception for a non-existant domain name" in {
    val thrown = evaluating { new DeleteDomain(data).handle(Map("DomainName" -> "abc"))} should produce [SDBException]
    thrown.httpStatus should equal (400)
    thrown.xmlCode should equal ("NoSuchDomain")
    thrown.message should equal ("The specified domain does not exist.")
  }
}