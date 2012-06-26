package fakesdb

import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.simpledb.AmazonSimpleDBClient
import com.amazonaws.services.simpledb.model._
import com.amazonaws.services.simpledb.model.{Attribute => AWSAttribute}
import org.scalatest.FeatureSpec
import org.scalatest.BeforeAndAfter
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.ShouldMatchers

class AttributeOperationsSpec extends FeatureSpec with BeforeAndAfter with GivenWhenThen with ShouldMatchers {

  val sdb = new AmazonSimpleDBClient(new BasicAWSCredentials("ignored", "ignored"))
  sdb.setEndpoint("http://127.0.0.1:8080")
  val domName = "mydom"

  before {
    sdb.createDomain(new CreateDomainRequest("_flush"))
    sdb.createDomain(new CreateDomainRequest(domName))
  }

  feature("The client can put, get, and delete attributes in an item") {

    scenario("a non-existent domain name is used") {
      when("a domain does not exist")

      then("a put attributes operation should throw an exception")
      evaluating {
        sdb.putAttributes(
          new PutAttributesRequest()
            .withDomainName("otherdom")
            .withItemName("item1")
            .withAttributes(new ReplaceableAttribute("attr1", "val1", false))
        )
      } should produce [NoSuchDomainException]

      and("a get attributes operation should throw an exception")
      evaluating {
        sdb.getAttributes(new GetAttributesRequest("otherdom", "item1"))
      } should produce [NoSuchDomainException]

      and("a delete attributes operation should throw an exception")
      evaluating {
        sdb.deleteAttributes(new DeleteAttributesRequest("otherdom", "item1"))
      } should produce [NoSuchDomainException]
    }

    scenario("a fresh item name is used") {
      val itemName = "item1"
      given("a fresh item name")
      val req = new PutAttributesRequest()
        .withDomainName(domName)
        .withItemName(itemName)
        .withAttributes(new ReplaceableAttribute("attr1", "val1", false))

      then("a put attributes operation should succeed")
      sdb.putAttributes(req)

      and("the new item can be got")
      val attrs = sdb.getAttributes(new GetAttributesRequest(domName, itemName)).getAttributes
      attrs should have size (1)
      attrs should contain (new AWSAttribute("attr1", "val1"))
    }

    scenario("a fresh attribute name for an exisiting item is used") {
      val itemName = "item1"
      given("an existing item")
      sdb.putAttributes(
        new PutAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new ReplaceableAttribute("attr1", "val1", false))
      )

      and("a fresh attribute name")
      val req = new PutAttributesRequest()
        .withDomainName(domName)
        .withItemName(itemName)
        .withAttributes(new ReplaceableAttribute("attr2", "val2", false))

      then("a put attributes operation should succeed")
      sdb.putAttributes(req)

      and("the extended item can be got")
      val attrs = sdb.getAttributes(new GetAttributesRequest(domName, itemName)).getAttributes
      attrs should have size (2)
      attrs should contain (new AWSAttribute("attr1", "val1"))
      attrs should contain (new AWSAttribute("attr2", "val2"))
    }

    scenario("an existing attribute is used, with the replace flag as false") {
      val itemName = "item1"
      given("an existing item")
      sdb.putAttributes(
        new PutAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new ReplaceableAttribute("attr1", "val1", false))
      )

      and("an exisiting attribute")
      val req = new PutAttributesRequest()
        .withDomainName(domName)
        .withItemName(itemName)
        .withAttributes(new ReplaceableAttribute("attr1", "val2", false))

      then("a put attributes operation should succeed")
      sdb.putAttributes(req)

      and("the extended attribute can be got")
      val attrs = sdb.getAttributes(
        new GetAttributesRequest(domName, itemName)
          .withAttributeNames("attr1")
      ).getAttributes
      attrs should have size (2)
      attrs should contain (new AWSAttribute("attr1", "val1"))
      attrs should contain (new AWSAttribute("attr1", "val2"))
    }

    scenario("an existing attribute is used, with the replace flag as true") {
      val itemName = "item1"
      given("an existing item")
      sdb.putAttributes(
        new PutAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new ReplaceableAttribute("attr1", "val1", false))
      )

      and("an exisiting attribute")
      val req = new PutAttributesRequest()
        .withDomainName(domName)
        .withItemName(itemName)
        .withAttributes(new ReplaceableAttribute("attr1", "val2", true))

      then("a put attributes operation should succeed")
      sdb.putAttributes(req)

      and("the updated attribute can be got")
      val attrs = sdb.getAttributes(
        new GetAttributesRequest(domName, itemName)
          .withAttributeNames("attr1")
      ).getAttributes
      attrs should have size (1)
      attrs should contain (new AWSAttribute("attr1", "val2"))
    }

    scenario("a non-existent item is to be deleted") {
      given("a non-existent item name")

      then("a delete attributes operation should still succeed")
      sdb.deleteAttributes(new DeleteAttributesRequest(domName, "noitem"))
    }

    scenario("an exisiting item is to be deleted") {
      val itemName = "item1"
      given("an existing item")
      sdb.putAttributes(
        new PutAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new ReplaceableAttribute("attr1", "val1", false))
      )

      then("a delete attributes operation should succeed")
      sdb.deleteAttributes(new DeleteAttributesRequest(domName, itemName))

      and("the item should no longer exist")
      val attrs = sdb.getAttributes(new GetAttributesRequest(domName, itemName)).getAttributes
      attrs should be ('empty)
    }

    scenario("a non-exisitant attribute on an item is to be deleted") {
      val itemName = "item1"
      given("an existing item")
      sdb.putAttributes(
        new PutAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new ReplaceableAttribute("attr1", "val1", false))
      )

      then("a delete attributes operation should succeed")
      sdb.deleteAttributes(
        new DeleteAttributesRequest(domName, itemName)
          .withAttributes(new AWSAttribute().withName("attr2"))
      )

      and("the item should remain as was")
      val attrs = sdb.getAttributes(new GetAttributesRequest(domName, itemName)).getAttributes
      attrs should have size (1)
      attrs should contain (new AWSAttribute("attr1", "val1"))
    }

    scenario("the sole attribute on an item is to be deleted") {
      val itemName = "item1"
      given("an existing item")
      sdb.putAttributes(
        new PutAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new ReplaceableAttribute("attr1", "val1", false))
      )

      then("a delete attributes operation should succeed")
      sdb.deleteAttributes(
        new DeleteAttributesRequest(domName, itemName)
          .withAttributes(new AWSAttribute().withName("attr1"))
      )

      and("the item should be deleted")
      val attrs = sdb.getAttributes(new GetAttributesRequest(domName, itemName)).getAttributes
      attrs should be ('empty)
    }

    scenario("one of two attributes on an item is to be deleted") {
      val itemName = "item1"
      given("an existing item")
      sdb.putAttributes(
        new PutAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new ReplaceableAttribute("attr1", "val1", false),
                          new ReplaceableAttribute("attr2", "val2", false))
      )

      then("a delete attributes operation should succeed")
      sdb.deleteAttributes(
        new DeleteAttributesRequest(domName, itemName)
          .withAttributes(new AWSAttribute().withName("attr2"))
      )

      and("the item with one attribute should remain")
      val attrs = sdb.getAttributes(new GetAttributesRequest(domName, itemName)).getAttributes
      attrs should have size (1)
      attrs should contain (new AWSAttribute("attr1", "val1"))
    }

    scenario("a value in a multi-valued attribute is to be deleted") {
      val itemName = "item1"
      given("an existing item")
      sdb.putAttributes(
        new PutAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new ReplaceableAttribute("attr1", "val1", false),
                          new ReplaceableAttribute("attr1", "val2", false))
      )

      then("a delete attributes operation should succeed")
      sdb.deleteAttributes(
        new DeleteAttributesRequest(domName, itemName)
          .withAttributes(new AWSAttribute().withName("attr1").withValue("val2"))
      )

      and("the item with one attribute should remain")
      val attrs = sdb.getAttributes(new GetAttributesRequest(domName, itemName)).getAttributes
      attrs should have size (1)
      attrs should contain (new AWSAttribute("attr1", "val1"))
    }

    scenario("a delete for a value without a name is requested") {
      val itemName = "item1"
      given("an existing item")
      sdb.putAttributes(
        new PutAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new ReplaceableAttribute("attr1", "val1", false))
      )

      when("a delete request is made with a value but no name")
      val req = new DeleteAttributesRequest(domName, itemName)
                  .withAttributes(new AWSAttribute().withValue("val1"))

      then("a delete attributes operation should throw an exception")
      evaluating {
        sdb.deleteAttributes(req)
      } should produce [MissingParameterException]
    }
  }
}
