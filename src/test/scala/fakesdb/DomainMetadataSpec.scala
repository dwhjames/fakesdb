package fakesdb

import fakesdb.actions._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class DomainMetadataSpec extends FlatSpec with ShouldMatchers {

  val data = new Data

  "The DomainMetadata action" should "throw an exception for missing DomainName param" in {
    evaluating { new DomainMetadata(data).handle(Map.empty) } should produce [MissingDomainNameException]
  }

  it should "throw an exception for a non-existant domain name" in {
    evaluating { new DomainMetadata(data).handle(Map("DomainName" -> "mydom")) } should produce [NoSuchDomainException]
  }

  it should "return zeros for an empty domain" in {
    data.getOrCreate("mydom")
    val xmlResp = new DomainMetadata(data).handle(Map("DomainName" -> "mydom"))
    (xmlResp \\ "ItemCount").text should equal ("0")
    (xmlResp \\ "ItemNamesSizeBytes").text should equal ("0")
    (xmlResp \\ "AttributeNameCount").text should equal ("0")
    (xmlResp \\ "AttributeNamesSizeBytes").text should equal ("0")
    (xmlResp \\ "AttributeValueCount").text should equal ("0")
    (xmlResp \\ "AttributeValuesSizeBytes").text should equal ("0")
  }

  it should "return calculated metadata for a domain" in {
    val domain = data.getOrCreate("mydom")
    val item1 = domain.getOrCreate("item1")
    item1.put("attr1", Set("val1", "val2"), false)
    item1.put("attr2", Set("val3", "val4"), false)
    val item2 = domain.getOrCreate("item2")
    item1.put("attr1", Set("val5"), false)

    val xmlResp = new DomainMetadata(data).handle(Map("DomainName" -> "mydom"))
    (xmlResp \\ "ItemCount").text should equal ("2")
    (xmlResp \\ "ItemNamesSizeBytes").text should equal ("10")
    (xmlResp \\ "AttributeNameCount").text should equal ("2")
    (xmlResp \\ "AttributeNamesSizeBytes").text should equal ("10")
    (xmlResp \\ "AttributeValueCount").text should equal ("5")
    (xmlResp \\ "AttributeValuesSizeBytes").text should equal ("20")
  }
}
