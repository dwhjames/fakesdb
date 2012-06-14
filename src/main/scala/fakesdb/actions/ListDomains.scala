package fakesdb.actions

import scala.xml
import fakesdb._

class ListDomains(data: Data) extends Action(data) {

  def handle(params: Params): xml.Node = {
    <ListDomainsResponse xmlns={namespace}>
      <ListDomainsResult>
      {for (domain <- data.getDomains) yield
        <DomainName>{domain.name}</DomainName>
      }
      </ListDomainsResult>
      {responseMetaData}
    </ListDomainsResponse>
  }

}
