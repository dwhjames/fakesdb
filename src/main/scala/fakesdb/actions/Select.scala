package fakesdb.actions

import scala.xml
import fakesdb._

class Select(data: Data) extends Action(data) {

  override def handle(params: Params): xml.Node = {
    val nextToken = try {
      params.get("NextToken").map(_.toInt).getOrElse(0)
    } catch { case e: NumberFormatException => throw new InvalidNextTokenException }
    if (nextToken < 0) throw new InvalidNextTokenException

    val itemsData = params.get("SelectExpression")
                          .map(SelectParser.makeSelectEval(_).select(data, nextToken))
                          .getOrElse(throw new SDBException(400, "Missing Parameter",
                                                            "The request must contain the parameter SelectExpression."))
    val items = itemsData._1
    val itemsLength = itemsData._2
    val newNextToken = if (itemsData._3) List(nextToken + itemsLength) else List()
    <SelectResponse xmlns={namespace}>
      <SelectResult>
        {for (item <- items) yield
          <Item>
            <Name>{item._1}</Name>
            {for (nv <- item._2) yield
              <Attribute><Name>{nv._1}</Name><Value>{nv._2}</Value></Attribute>
            }
          </Item>
        }
        {for (token <- newNextToken) yield
          <NextToken>{token}</NextToken>
        }
      </SelectResult>
      {responseMetaData}
    </SelectResponse>
  }

}
