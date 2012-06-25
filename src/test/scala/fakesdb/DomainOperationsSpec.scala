package fakesdb

import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.simpledb.AmazonSimpleDBClient
import com.amazonaws.services.simpledb.model._
import org.scalatest.FeatureSpec
import org.scalatest.BeforeAndAfter
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.ShouldMatchers

class DomainOperationsSpec extends FeatureSpec with BeforeAndAfter with GivenWhenThen with ShouldMatchers {

  val sdb = new AmazonSimpleDBClient(new BasicAWSCredentials("ignored", "ignored"))
  sdb.setEndpoint("http://127.0.0.1:8080")

  before {
    sdb.createDomain(new CreateDomainRequest("_flush"))
  }

  feature("The client can create a new domain") {

    scenario("a invalid domain name is used") {
      val name = "bad!domain?name"
      given("an invalid domain name: \"%s\"".format(name))
      then("an exception should be thrown")
      evaluating {
        sdb.createDomain(new CreateDomainRequest(name))
      } should produce [InvalidParameterValueException]
    }

    scenario("a valid domain name is used") {
      val name = "mydom"
      given("a valid domain name: \"%s\"".format(name))

      when("the domain does not exist")
      sdb.listDomains().getDomainNames should not contain (name)

      then("a create domain operation should succeed")
      sdb.createDomain(new CreateDomainRequest(name))

      and("a new domain should be present")
      sdb.listDomains().getDomainNames should contain (name)
    }

    scenario("an exisiting domain name is used") {
      val name = "mydom"
      given("a valid domain name: \"%s\"".format(name))

      when("the domain already exists")
      sdb.createDomain(new CreateDomainRequest(name))
      sdb.listDomains().getDomainNames should contain (name)

      then("a create domain operation should still succeed")
      sdb.createDomain(new CreateDomainRequest(name))
    }
  }

  feature("The client can delete a domain") {

    scenario("the domain exists") {
      val name = "mydom"
      given("an exisiting domain: \"%s\"".format(name))
      sdb.createDomain(new CreateDomainRequest(name))
      sdb.listDomains().getDomainNames should contain (name)

      then("a delete domain operation should succeed")
      sdb.deleteDomain(new DeleteDomainRequest(name))

      and("the domain should no longer be present")
      sdb.listDomains().getDomainNames should not contain (name)
    }

    scenario("the domain does not exist") {
      val name = "mydom"
      given("a valid domain name: \"%s\"".format(name))

      when("the domain does not exist")
      sdb.listDomains().getDomainNames should not contain (name)

      then("a delete domain operation should still succeed")
      sdb.deleteDomain(new DeleteDomainRequest(name))
    }
  }

  feature("The client can list the domains") {

    scenario("no domains exist") {
      given("that no domains exists")
      then("a list domains operation should succeed")
      val names =  sdb.listDomains().getDomainNames

      and("should return an empty result")
      names should be ('empty)
    }

    val namePrefix = "dom"
    val total = 5
    def createBatchOfDomains() =
      for (i <- 1 to total)
        sdb.createDomain(new CreateDomainRequest("%s%d".format(namePrefix, i)))

    scenario("%d domains exist, and all are required".format(total)) {
      given("a set of initial domains: \"%s%d\" to \"%s%d\"".format(namePrefix, 1, namePrefix, total))
      createBatchOfDomains()

      then("a list domains operation should succeed")
      val names =  sdb.listDomains().getDomainNames

      and("should return %d results".format(total))
      names should have size (total)
      for (i <- 1 to total)
        names should contain ("%s%d".format(namePrefix, i))
    }

    val limit = 3

    scenario("%d domains exists, and the first %d are required".format(total, limit)) {
      given("a set of initial domains: \"%s%d\" to \"%s%d\"".format(namePrefix, 1, namePrefix, total))
      createBatchOfDomains()

      then("a list domains operation should succeed")
      val req = new ListDomainsRequest()
        .withMaxNumberOfDomains(limit)
      val names =  sdb.listDomains(req).getDomainNames

      and("should return %d results".format(limit))
      names should have size (limit)
      for (i <- 1 to limit)
        names should contain ("%s%d".format(namePrefix, i))
    }

    scenario("%d domains exists, and the last %d are required".format(total, total - limit)) {
      given("a set of initial domains: \"%s%d\" to \"%s%d\"".format(namePrefix, 1, namePrefix, total))
      createBatchOfDomains()

      and("a NextToken after the first %d results".format(limit))
      var req = new ListDomainsRequest()
        .withMaxNumberOfDomains(limit)
      val tok =  sdb.listDomains(req).getNextToken()

      then("a list domains operation should succeed")
      req = new ListDomainsRequest()
        .withNextToken(tok)
      val names =  sdb.listDomains(req).getDomainNames

      and("should return %d results".format(total - limit))
      names should have size (total - limit)
      for (i <- limit + 1 to total)
        names should contain ("%s%d".format(namePrefix, i))
    }
  }
}
