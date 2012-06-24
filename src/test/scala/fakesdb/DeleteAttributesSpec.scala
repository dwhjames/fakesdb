package fakesdb

import fakesdb.actions._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class DeleteAttributesSpec extends FlatSpec with ShouldMatchers {

  val data = new Data
  val longName = "a" * (Limits.ParamValMaxLen + 1)
  val domain = data.getOrCreate("mydom")
  val item = domain.getOrCreate("myitem")

  "The DeleteAttributes action" should "throw an exception for missing DomainName param" in {
    evaluating { new DeleteAttributes(data).handle(Map.empty) } should produce [MissingDomainNameException]
  }

  it should "throw an exception for a non-existant domain name" in {
    evaluating { new DeleteAttributes(data).handle(Map("DomainName" -> "abc"))} should produce [NoSuchDomainException]
  }

  it should "throw an exception for missing ItemName param" in {
    evaluating { new DeleteAttributes(data).handle(Map("DomainName" -> "mydom")) } should produce [MissingItemNameException]
  }

  it should "throw an exception for an ItemName param that is empty" in {
    evaluating { new DeleteAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> "")) } should produce [InvalidParameterValue]
  }

  it should "throw an exception for an ItemName param that is too long" in {
    evaluating { new DeleteAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> longName)) } should produce [InvalidParameterValue]
  }

  it should "throw an exception for an Attribute.Name value that is empty" in {
    evaluating { new DeleteAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> "myitem", "Attribute.0.Name" -> "")) } should produce [InvalidParameterValue]
  }

  it should "throw an exception for an Attribute.Name value that is too long" in {
    evaluating { new DeleteAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> "myitem", "Attribute.0.Name" -> longName)) } should produce [InvalidParameterValue]
  }

  it should "throw an exception for an Attribute.Value value that is too long" in {
    evaluating { new DeleteAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> "myitem", "Attribute.0.Name" -> "attr1", "Attribute.0.Value" -> longName)) } should produce [InvalidParameterValue]
  }

  it should "throw an exception for a missing Attribute.Name parameter" in {
    val thrown = evaluating { new DeleteAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> "myitem", "Attribute.0.Value" -> "1")) } should produce [SDBException]
    thrown.xmlCode should equal ("MissingParameter")
  }

  it should "discover an item deletion" in {
    var upd = new DeleteAttributes(data).discoverAttributes("myitem", Map())
    upd.size should equal (1)
    var attrs = upd("myitem")
    attrs.size should equal (0)
  }

  it should "discover an attribute deletion" in {
    var upd = new DeleteAttributes(data).discoverAttributes("myitem", Map("Attribute.0.Name" -> "attr1"))
    upd.size should equal (1)
    var attrs = upd("myitem")
    attrs.size should equal (1)
    attrs("attr1").values should be ('empty)
  }

  it should "discover an attribute deletion indexed from 1 rather than 0" in {
    var upd = new DeleteAttributes(data).discoverAttributes("myitem", Map("Attribute.1.Name" -> "attr1"))
    upd.size should equal (1)
    var attrs = upd("myitem")
    attrs.size should equal (1)
    attrs("attr1").values should be ('empty)
  }

  it should "discover multiple attribute deletions" in {
    var upd = new DeleteAttributes(data).discoverAttributes("myitem", Map("Attribute.0.Name" -> "attr1", "Attribute.1.Name" -> "attr2"))
    upd.size should equal (1)
    var attrs = upd("myitem")
    attrs.size should equal (2)
    attrs("attr1").values should be ('empty)
    attrs("attr2").values should be ('empty)
  }

  it should "discover an attribute value deletion" in {
    var upd = new DeleteAttributes(data).discoverAttributes("myitem", Map("Attribute.0.Name" -> "attr1", "Attribute.0.Value" -> "val1"))
    upd.size should equal (1)
    var attrs = upd("myitem")
    attrs.size should equal (1)
    attrs("attr1").values should be (Set("val1"))
  }

  it should "discover multiple attribute value deletions" in {
    var upd = new DeleteAttributes(data).discoverAttributes("myitem", Map("Attribute.0.Name" -> "attr1", "Attribute.0.Value" -> "val1", "Attribute.1.Name" -> "attr1", "Attribute.1.Value" -> "val2"))
    upd.size should equal (1)
    var attrs = upd("myitem")
    attrs.size should equal (1)
    attrs("attr1").values should be (Set("val1", "val2"))
  }
}
