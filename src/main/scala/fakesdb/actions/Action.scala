package fakesdb.actions

import scala.xml
import fakesdb._

abstract class Action(data: Data) {

  def handle(params: Params): xml.Node

  protected def responseMetaData() = {
    <ResponseMetadata><RequestId>{requestId}</RequestId><BoxUsage>0</BoxUsage></ResponseMetadata>
  }

  protected def parseDomain(params: Params): Domain = {
    val domainName = params.getOrElse("DomainName", throw new MissingDomainNameException)
    data.get(domainName)
  }

  val namespace = "http://sdb.amazonaws.com/doc/2009-04-15/"

  val requestId = Action.requestCounter.incrementAndGet()

}

object Action {
  private val requestCounter = new java.util.concurrent.atomic.AtomicInteger()
}
