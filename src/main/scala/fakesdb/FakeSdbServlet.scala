package fakesdb

import javax.servlet.http._
import fakesdb.actions._

class FakeSdbServlet extends HttpServlet {

  val data = new Data

  override def doGet(request: HttpServletRequest, response: HttpServletResponse): Unit = synchronized {
    val params = Params(request)

    var xml = ""
    try {
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
      xml = action.handle(params).toString
    } catch {
      case e => {
        xml = toXML(e).toString
        response.setStatus(e match {
          case se: SDBException => se.httpStatus
          case _ => 400
        })
      }
    }

    response.setContentType("text/xml")
    response.getWriter.write(xml)
  }

  private def toXML(t: Throwable) = {
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
