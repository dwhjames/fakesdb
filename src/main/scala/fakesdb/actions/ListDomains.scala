package fakesdb.actions

import scala.xml
import fakesdb._

class ListDomains(data: Data) extends Action(data) {

  def handle(params: Params): xml.Node = {
    val strTotal = params.get("MaxNumberOfDomains").getOrElse("100")
    val exception = new InvalidParameterValue("Value (%s) for parameter MaxNumberOfDomains is invalid. MaxNumberOfDomains must be between 1 and 100.".format(strTotal))
    val total: Int = try strTotal.toInt catch { case e: NumberFormatException => throw exception }
    if (total < 1 || 100 < total) throw exception

    val nextToken = try {
      params.get("NextToken").map(_.toInt).getOrElse(0)
    } catch { case e: NumberFormatException => throw new InvalidNextTokenException }
    if (nextToken < 0) throw new InvalidNextTokenException

    val newNextToken = nextToken + total
    val it = data.iterator.slice(nextToken, newNextToken)
    if (nextToken > 0 && it.isEmpty) throw new InvalidNextTokenException

    <ListDomainsResponse xmlns={namespace}>
      <ListDomainsResult>
      {
        for (domain <- it) yield
          <DomainName>{domain.name}</DomainName>
      }
      {
        if (newNextToken < data.size)
          <NextToken>{newNextToken}</NextToken>
      }
      </ListDomainsResult>
      {responseMetaData}
    </ListDomainsResponse>
  }

}
