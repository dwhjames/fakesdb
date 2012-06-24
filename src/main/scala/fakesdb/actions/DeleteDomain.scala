package fakesdb.actions

import scala.xml
import fakesdb._

class DeleteDomain(data: Data) extends Action(data) {
  
  def handle(params: Params): xml.Node = {
    val domainName = params.getOrElse("DomainName", throw new MissingDomainNameException)
    data.get(domainName) foreach { data.remove(_) }
    <DeleteDomainResponse xmlns={namespace}>
      {responseMetaData}
    </DeleteDomainResponse>
  }

}
