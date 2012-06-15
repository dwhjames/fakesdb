package fakesdb.actions

import scala.xml
import fakesdb._

class DeleteDomain(data: Data) extends Action(data) {
  
  def handle(params: Params): xml.Node = {
    data.remove(parseDomain(params))
    <DeleteDomainResponse xmlns={namespace}>
      {responseMetaData}
    </DeleteDomainResponse>
  }

}
