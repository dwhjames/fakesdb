package fakesdb

import fakesdb.actions._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.xml.NodeSeq

class GetAttributesSpec extends FlatSpec with ShouldMatchers {

  val data = new Data
  val longName = "a" * (Limits.ParamValMaxLen + 1)
  val domain = data.getOrCreate("mydom")
  val item = domain.getOrCreate("myitem")
  val attrs = Set("attr1" -> "val1", "attr2" -> "val2", "attr3" -> "val3a", "attr3" -> "val3b")

  {
    for ((n, v) <- attrs) item.put(n, v, false)
  }

  "The GetAttributes action" should "throw an exception for missing DomainName param" in {
    evaluating { new GetAttributes(data).handle(Map.empty) } should produce [MissingDomainNameException]
  }

  it should "throw an exception for a non-existent domain name" in {
    evaluating { new GetAttributes(data).handle(Map("DomainName" -> "nodom"))} should produce [NoSuchDomainException]
  }

  it should "throw an exception for missing ItemName param" in {
    evaluating { new GetAttributes(data).handle(Map("DomainName" -> "mydom")) } should produce [MissingItemNameException]
  }

  it should "throw an exception for an ItemName param that is empty" in {
    evaluating { new GetAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> "")) } should produce [InvalidParameterValue]
  }

  it should "throw an exception for an ItemName param that is too long" in {
    evaluating { new GetAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> longName)) } should produce [InvalidParameterValue]
  }

  it should "throw an exception for an AttributeName param that is empty" in {
    evaluating { new GetAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> "myItem", "AttributeName.0" -> "")) } should produce [InvalidParameterValue]
  }

  it should "throw an exception for an AttributeName param that is too long" in {
    evaluating { new GetAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> "myItem", "AttributeName.0" -> longName)) } should produce [InvalidParameterValue]
  }

  it should "return a empty result for a non-existent item name" in {
    val xmlResp = new GetAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> "noitem"))
    (xmlResp \ "GetAttributesResult" \ "_") should be ('empty)
  }

  private def extractAttrs(attrNodes: NodeSeq): Set[(String, String)] =
    (for {
       node <- attrNodes
       n = (node \ "Name").text
       v = (node \ "Value").text
     } yield (n, v)).toSet

  it should "return all attributes if none are specified" in {
    val xmlResp = new GetAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> "myitem"))
    extractAttrs(xmlResp \\ "Attribute") should equal (attrs)
  }

  it should "return only the specified attributes" in {
    var xmlResp = new GetAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> "myitem", "AttributeName.0" -> "attr2", "AttributeName.1" -> "attr1"))
    extractAttrs(xmlResp \\ "Attribute") should equal (Set("attr1" -> "val1", "attr2" -> "val2"))

    xmlResp = new GetAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> "myitem", "AttributeName.0" -> "attr3"))
    extractAttrs(xmlResp \\ "Attribute") should equal (Set("attr3" -> "val3a", "attr3" -> "val3b"))
  }

  it should "allow AttributeName params to count from 0 or 1" in {
    var xmlResp = new GetAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> "myitem", "AttributeName.0" -> "attr2"))
    extractAttrs(xmlResp \\ "Attribute") should equal (Set("attr2" -> "val2"))

    xmlResp = new GetAttributes(data).handle(Map("DomainName" -> "mydom", "ItemName" -> "myitem", "AttributeName.1" -> "attr2"))
    extractAttrs(xmlResp \\ "Attribute") should equal (Set("attr2" -> "val2"))
  }
}
