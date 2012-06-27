package fakesdb

import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.simpledb.AmazonSimpleDBClient
import com.amazonaws.services.simpledb.model._
import com.amazonaws.services.simpledb.model.{Item => AWSItem}
import org.scalatest.FeatureSpec
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.ShouldMatchers
import scala.collection.JavaConverters._

class ClientQuerySpec extends FeatureSpec with ShouldMatchers with BeforeAndAfter with FeatureSpecQueryBehaviors {

  val sdb = new AmazonSimpleDBClient(new BasicAWSCredentials("ignored", "ignored"))
  sdb.setEndpoint("http://127.0.0.1:8080")

  before {
    sdb.createDomain(new CreateDomainRequest("_flush"))
    sdb.createDomain(new CreateDomainRequest(TestData.domainName))

    for ((itemName, attrs) <- TestData.items) {
      sdb.putAttributes(
        new PutAttributesRequest()
          .withDomainName(TestData.domainName)
          .withItemName(itemName)
          .withAttributes(
            (for ((name, value) <- attrs) yield
               new ReplaceableAttribute(name, value, false)
            ).asJavaCollection
          )
      )
    }
  }

  private def queryItemNames(query: String): Seq[String] =
    sdb.select(new SelectRequest(query)).getItems.asScala.map(_.getName)

  feature("Simple Queries") {
    scenariosFor(simpleQueries(queryItemNames))
  }

  feature("Range Queries") {
    scenariosFor(rangeQueries(queryItemNames))
  }

  feature("Queries on Attributes with Multiple Values") {
    scenariosFor(multiValuedAttributeQueries(queryItemNames))
  }

  feature("Multiple Attribute Queries") {
    scenariosFor(multiAttributeQueries(queryItemNames))
  }

  feature("Sort Queries") {
    scenariosFor(sortQueries(queryItemNames))
  }
}
