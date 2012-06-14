package fakesdb

import fakesdb.actions._
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import scala.xml


class FakeSdbServlet extends HttpServlet {

  val data = new Data

  override def doGet(request: HttpServletRequest, response: HttpServletResponse): Unit = synchronized {
    try {
      val params = parseParams(request)
      if (!params.contains("Action"))
        throw new SDBException(400, "MissingAction", "No action was supplied with this request.")
      val action = params("Action") match {
        case "CreateDomain" => new CreateDomain(data)
        case "DeleteDomain" => new DeleteDomain(data)
        case "DomainMetadata" => new DomainMetadata(data)
        case "ListDomains" => new ListDomains(data)
        case "GetAttributes" => new GetAttributes(data)
        case "PutAttributes" => new PutAttributes(data)
        case "BatchPutAttributes" => new BatchPutAttributes(data)
        case "BatchDeleteAttributes" => new BatchDeleteAttributes(data)
        case "DeleteAttributes" => new DeleteAttributes(data)
        case "Select" => new Select(data)
        case other =>
          throw new SDBException(400, "InvalidAction", "The action %s is not valid for this web service.".format(other))
      }
      serveXML(response, action.handle(params))
    } catch {
      case e => {
        response.setStatus(e match {
          case se: SDBException => se.httpStatus
          case _ => 400
        })
        serveXML(response, toXML(e))
      }
    }
  }

  private def parseParams(request: HttpServletRequest): Params = {
    val p = new scala.collection.mutable.HashMap[String,String]
    val i = request.getParameterMap.asInstanceOf[java.util.Map[String, Array[String]]].entrySet.iterator
    while (i.hasNext) {
      val e = i.next
      p.update(e.getKey(), e.getValue()(0))
    }
    p.toMap
  }

  private def toXML(t: Throwable): xml.Node = {
    val xmlCode = t match {
      case se: SDBException => se.xmlCode
      case _ => "InternalError"
    }

    <Response>
      <Errors>
        <Error>
          <Code>{xmlCode}</Code>
          <Message>{t.getMessage}</Message>
          <BoxUsage>0</BoxUsage>
        </Error>
      </Errors>
      <RequestId>0</RequestId>
    </Response>
  }

  private def serveXML(response: HttpServletResponse, xmlNode: xml.Node): Unit = {
    response.setContentType("text/xml")
    xml.XML.write(response.getWriter, xmlNode, "utf-8", true, null)
  }

  override def doPost(request: HttpServletRequest, response: HttpServletResponse): Unit = doGet(request, response)

  override def doPut(request: HttpServletRequest, response: HttpServletResponse): Unit = unsupportedHttpVerb(response, "PUT")

  override def doDelete(request: HttpServletRequest, response: HttpServletResponse): Unit = unsupportedHttpVerb(response, "DELETE")

  private def unsupportedHttpVerb(response: HttpServletResponse, verb: String): Unit = {
    response.setStatus(400)
    response.setContentType("text/xml")
    response.getWriter.write(
      toXML(new SDBException(400, "UnsupportedHttpVerb", "The requested HTTP verb is not supported: %s.".format(verb))).toString)
  }
}
