package fakesdb

import fakesdb.actions._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class PutAttributesSpec extends FlatSpec with ShouldMatchers {

  val data = new Data
  val longName = "a" * (Limits.ParamValMaxLen + 1)
  val domain = data.getOrCreate("mydom")

  "The PutAttributes action" should "throw an exception for missing DomainName param" in {
    evaluating { new PutAttributes(data).handle(Map.empty) } should produce [MissingDomainNameException]
  }

  it should "throw an exception for a non-existent domain name" in {
    evaluating { new PutAttributes(data).handle(Map("DomainName" -> "abc"))} should produce [NoSuchDomainException]
  }

  it should "throw an exception for missing ItemName param" in {
    evaluating { new PutAttributes(data).handle(Map("DomainName" -> "mydom")) } should produce [MissingItemNameException]
  }

  it should "throw an exception for an ItemName param that is empty" in {
    evaluating { new PutAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> "")) } should produce [InvalidParameterValue]
  }

  it should "throw an exception for an ItemName param that is too long" in {
    evaluating { new PutAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> longName)) } should produce [InvalidParameterValue]
  }

  it should "throw an exception for a missing Attribute.Name parameter" in {
    val thrown = evaluating { new PutAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> "newitem")) } should produce [SDBException]
    thrown.xmlCode should equal ("MissingParameter")
    domain.get("newitem") should be ('empty)
  }

  it should "throw an exception for an Attribute.Name value that is empty" in {
    evaluating { new PutAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> "myitem", "Attribute.0.Name" -> "", "Attribute.0.Value" -> "1")) } should produce [InvalidParameterValue]
  }

  it should "throw an exception for an Attribute.Name value that is too long" in {
    evaluating { new PutAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> "myitem", "Attribute.0.Name" -> longName, "Attribute.0.Value" -> "1")) } should produce [InvalidParameterValue]
  }

  it should "throw an exception for an Attribute.Value value that is too long" in {
    evaluating { new PutAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> "myitem", "Attribute.0.Name" -> "attr1", "Attribute.0.Value" -> longName)) } should produce [InvalidParameterValue]
  }

  it should "throw an exception for an Attribute.Replace value that is neither \"true\" nor \"false\"" in {
    evaluating { new PutAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> "myitem", "Attribute.0.Name" -> "attr1", "Attribute.0.Value" -> "1", "Attribute.0.Replace" -> "foo")) } should produce [InvalidParameterValue]
  }

  it should "throw an exception for an unmatched missing Attribute.Value parameter" in {
    val thrown = evaluating { new PutAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> "myitem", "Attribute.0.Value" -> "1")) } should produce [SDBException]
    thrown.xmlCode should equal ("MissingParameter")
  }

  it should "throw an exception for an unmatched Attribute.Name parameter" in {
    val thrown = evaluating { new PutAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> "myitem", "Attribute.0.Name" -> "attr1")) } should produce [SDBException]
    thrown.xmlCode should equal ("MissingParameter")
  }

  it should "throw an exception for too many submitted attributes for an item" in {
    var params = Map("DomainName" -> "mydom", "ItemName" -> "myitem")
    for (i <- 0 to Limits.MaxNameValPairsPerItem) {
      params += ("Attribute.%d.Name".format(i)  -> "attr%d".format(i),
                 "Attribute.%d.Value".format(i) -> "val%d".format(i))
    }
    val thrown = evaluating { new PutAttributes(data).handle(params) } should produce [SDBException]
    thrown.xmlCode should equal ("NumberSubmittedAttributesExceeded")
  }

  it should "throw an exception for too many attributes for an item" in {
    var params = Map("DomainName" -> "mydom", "ItemName" -> "myitem",
                     "Attribute.0.Name"  -> "over_lim", "Attribute.0.Value" -> "over_lim")
    val data = new Data
    val item = data.getOrCreate("mydom").getOrCreate("myitem")
    for (i <- 0 until Limits.MaxNameValPairsPerItem) {
      item.put("attr%d".format(i), "val%d".format(i), true)
    }
    val thrown = evaluating { new PutAttributes(data).handle(params) } should produce [SDBException]
    thrown.xmlCode should equal ("NumberItemAttributesExceeded")
  }

  it should "discover an attribute update and default to Replace=false" in {
    var upd = new PutAttributes(data).discoverAttributes("myitem", Map("Attribute.0.Name" -> "attr1", "Attribute.0.Value" -> "val1"))
    upd.size should equal (1)
    var attrs = upd("myitem")
    attrs.size should equal (1)
    attrs("attr1").replace should equal (false)
    attrs("attr1").values should equal (Set("val1"))
  }

  it should "discover an attribute update indexed from 1 rather than 0" in {
    var upd = new PutAttributes(data).discoverAttributes("myitem", Map("Attribute.1.Name" -> "attr1", "Attribute.1.Value" -> "val1", "Attribute.1.Replace" -> "true"))
    upd.size should equal (1)
    var attrs = upd("myitem")
    attrs.size should equal (1)
    attrs("attr1").replace should equal (true)
    attrs("attr1").values should equal (Set("val1"))
  }

  it should "discover multiple attribute updates" in {
    var upd = new PutAttributes(data).discoverAttributes("myitem", Map("Attribute.1.Name" -> "attr1", "Attribute.1.Value" -> "val1", "Attribute.2.Name" -> "attr2", "Attribute.2.Value" -> "val2"))
    upd.size should equal (1)
    var attrs = upd("myitem")
    attrs.size should equal (2)
    attrs("attr1").replace should equal (false)
    attrs("attr1").values should equal (Set("val1"))
    attrs("attr2").replace should equal (false)
    attrs("attr2").values should equal (Set("val2"))
  }

  it should "discover a multi-valued attribute update" in {
    var upd = new PutAttributes(data).discoverAttributes("myitem", Map("Attribute.1.Name" -> "attr1", "Attribute.1.Value" -> "val1", "Attribute.2.Name" -> "attr1", "Attribute.2.Value" -> "val2"))
    upd.size should equal (1)
    var attrs = upd("myitem")
    attrs.size should equal (1)
    attrs("attr1").replace should equal (false)
    attrs("attr1").values should equal (Set("val1", "val2"))
  }

  it should "ensure that name-value attribute pairs are unique" in {
    var upd = new PutAttributes(data).discoverAttributes("myitem", Map("Attribute.1.Name" -> "attr1", "Attribute.1.Value" -> "val1", "Attribute.2.Name" -> "attr1", "Attribute.2.Value" -> "val1"))
    upd.size should equal (1)
    var attrs = upd("myitem")
    attrs.size should equal (1)
    attrs("attr1").replace should equal (false)
    attrs("attr1").values should equal (Set("val1"))
  }
}
