package fakesdb

import com.amazonaws.AmazonServiceException
import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.simpledb.AmazonSimpleDBClient
import com.amazonaws.services.simpledb.model._
import com.amazonaws.services.simpledb.model.{Attribute => AWSAttribute}
import org.scalatest.FeatureSpec
import org.scalatest.BeforeAndAfter
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.ShouldMatchers

class ConditionalOperationsSpec extends FeatureSpec with BeforeAndAfter with GivenWhenThen with ShouldMatchers {

  val sdb = new AmazonSimpleDBClient(new BasicAWSCredentials("ignored", "ignored"))
  sdb.setEndpoint("http://127.0.0.1:8080")
  val domName = "mydom"

  before {
    sdb.createDomain(new CreateDomainRequest("_flush"))
    sdb.createDomain(new CreateDomainRequest(domName))
  }

  feature("The client can put attributes conditionally") {

    scenario("an existing attribute is to be updated, conditional on the old value") {
      val itemName = "item1"
      given("an existing item with an attribute")
      sdb.putAttributes(
        new PutAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new ReplaceableAttribute("attr1", "val1", false))
      )

      then("a put expecting the old value should succeed")
      sdb.putAttributes(
        new PutAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new ReplaceableAttribute("attr1", "val2", true))
          .withExpected(new UpdateCondition().withName("attr1").withValue("val1"))
      )

      and("the updated item can be got")
      val attrs = sdb.getAttributes(new GetAttributesRequest(domName, itemName)).getAttributes
      attrs should have size (1)
      attrs should contain (new AWSAttribute("attr1", "val2"))
    }

    scenario("an item is to be added, if it doesn't exist") {
      val itemName = "item1"
      given("a non-existent item name")

      then("a put expecting any attribute's absence should succeed")
      sdb.putAttributes(
        new PutAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new ReplaceableAttribute("attr1", "val1", false))
          .withExpected(new UpdateCondition().withName("attr1").withExists(false))
      )

      and("the new item can be got")
      val attrs = sdb.getAttributes(new GetAttributesRequest(domName, itemName)).getAttributes
      attrs should have size (1)
      attrs should contain (new AWSAttribute("attr1", "val1"))
    }

    scenario("an attribute is to be added, if it doesn't exist") {
      val itemName = "item1"
      given("an item with an attribute")
      sdb.putAttributes(
        new PutAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new ReplaceableAttribute("attr1", "val1", false))
      )

      then("a put expecting another attribute's absence should succeed")
      sdb.putAttributes(
        new PutAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new ReplaceableAttribute("attr2", "val2", false))
          .withExpected(new UpdateCondition().withName("attr2").withExists(false))
      )

      and("the extended item can be got")
      val attrs = sdb.getAttributes(new GetAttributesRequest(domName, itemName)).getAttributes
      attrs should have size (2)
      attrs should contain (new AWSAttribute("attr1", "val1"))
      attrs should contain (new AWSAttribute("attr2", "val2"))
    }

    scenario("an attribute unexpectedly exists") {
      val itemName = "item1"
      given("an item with an attribute")
      sdb.putAttributes(
        new PutAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new ReplaceableAttribute("attr1", "val1", false))
      )

      then("a put expecting that attribute's absence should throw an exception")
      val thrown = evaluating {
        sdb.putAttributes(
          new PutAttributesRequest()
            .withDomainName(domName)
            .withItemName(itemName)
            .withAttributes(new ReplaceableAttribute("attr2", "val2", false))
            .withExpected(new UpdateCondition().withName("attr1").withExists(false))
        )
      } should produce [AmazonServiceException]
      thrown.getErrorCode should equal ("ConditionalCheckFailed")
    }

    scenario("an attribute with a non-matching value") {
      val itemName = "item1"
      given("an exisiting item with an attribute")
      sdb.putAttributes(
        new PutAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new ReplaceableAttribute("attr1", "val1", false))
      )

      then("a put expecting a different value should throw an exception")
      val thrown = evaluating {
        sdb.putAttributes(
          new PutAttributesRequest()
            .withDomainName(domName)
            .withItemName(itemName)
            .withAttributes(new ReplaceableAttribute("attr1", "val3", false))
            .withExpected(new UpdateCondition().withName("attr1").withValue("val2"))
        )
      } should produce [AmazonServiceException]
      thrown.getErrorCode should equal ("ConditionalCheckFailed")
    }

    scenario("an expected attribute does not exist") {
      val itemName = "item1"
      given("an item with an attribute")
      sdb.putAttributes(
        new PutAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new ReplaceableAttribute("attr1", "val1", false))
      )

      then("a put expecting another attribute should throw an exception")
      val thrown = evaluating {
        sdb.putAttributes(
          new PutAttributesRequest()
            .withDomainName(domName)
            .withItemName(itemName)
            .withAttributes(new ReplaceableAttribute("attr1", "val3", false))
            .withExpected(new UpdateCondition().withName("attr2").withValue("val2"))
        )
      } should produce [AmazonServiceException]
      thrown.getErrorCode should equal ("AttributeDoesNotExist")
    }

    scenario("an attribute with multiple values") {
      val itemName = "item1"
      given("an item with a multi-valued attribute")
      sdb.putAttributes(
        new PutAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new ReplaceableAttribute("attr1", "val1", false),
                          new ReplaceableAttribute("attr1", "val2", false))
      )

      then("a put expecting a single value should throw an exception")
      val thrown = evaluating {
        sdb.putAttributes(
          new PutAttributesRequest()
            .withDomainName(domName)
            .withItemName(itemName)
            .withAttributes(new ReplaceableAttribute("attr1", "val3", false))
            .withExpected(new UpdateCondition().withName("attr1").withValue("val2"))
        )
      } should produce [AmazonServiceException]
      thrown.getErrorCode should equal ("MultiValuedAttribute")
    }
  }

  feature("The client can delete items and attributes conditionally") {

    scenario("an item is to be deleted, conditional on its attribute") {
      val itemName = "item1"
      given("an item with an attribute")
      sdb.putAttributes(
        new PutAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new ReplaceableAttribute("attr1", "val1", false))
      )

      then("a delete conditional on that attribute should succeed")
      sdb.deleteAttributes(
        new DeleteAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withExpected(new UpdateCondition().withName("attr1").withValue("val1"))
      )

      and("the item should be deleted")
      val attrs = sdb.getAttributes(new GetAttributesRequest(domName, itemName)).getAttributes
      attrs should be ('empty)
    }

    scenario("an attribute is to be deleted, conditional on its value") {
      val itemName = "item1"
      given("an item with two attributes")
      sdb.putAttributes(
        new PutAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new ReplaceableAttribute("attr1", "val1", false),
                          new ReplaceableAttribute("attr2", "val2", false))
      )

      then("a delete conditional on the second should succeed")
      sdb.deleteAttributes(
        new DeleteAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new AWSAttribute().withName("attr2"))
          .withExpected(new UpdateCondition().withName("attr2").withValue("val2"))
      )

      and("the first attribute should remain")
      val attrs = sdb.getAttributes(new GetAttributesRequest(domName, itemName)).getAttributes
      attrs should have size (1)
      attrs should contain (new AWSAttribute("attr1", "val1"))
    }

    scenario("a value is to be deleted, conditional on another attribute") {
      val itemName = "item1"
      given("an item with a multi-valued attribute")
      sdb.putAttributes(
        new PutAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new ReplaceableAttribute("attr1", "val1", false),
                          new ReplaceableAttribute("attr1", "val2", false),
                          new ReplaceableAttribute("attr1", "val3", false))
      )

      then("a delete expecting the absence of another attribute should succeed")
      sdb.deleteAttributes(
        new DeleteAttributesRequest()
          .withDomainName(domName)
          .withItemName(itemName)
          .withAttributes(new AWSAttribute().withName("attr1").withValue("val2"))
          .withExpected(new UpdateCondition().withName("attr2").withExists(false))
      )

      and("the other values should remain")
      val attrs = sdb.getAttributes(new GetAttributesRequest(domName, itemName)).getAttributes
      attrs should have size (2)
      attrs should contain (new AWSAttribute("attr1", "val1"))
      attrs should contain (new AWSAttribute("attr1", "val3"))
    }
  }
}
