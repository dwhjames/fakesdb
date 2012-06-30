package fakesdb.actions

import java.nio.ByteBuffer
import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter
import scala.xml
import fakesdb._

class Select(data: Data) extends Action(data) {

  override def handle(params: Params): xml.Node = {
    val query = params.get("SelectExpression")
                      .getOrElse(throw new SDBException(400, "Missing Parameter",
                                 "The request must contain the parameter SelectExpression."))
    val queryDigest = MessageDigest.getInstance("MD5").digest(query.getBytes("UTF-8"))

    val nextToken = try {
      params.get("NextToken").map(str => {
        val tokenBytes = DatatypeConverter.parseBase64Binary(str)
        if ((tokenBytes.length != 20)) throw new InvalidNextTokenException
        val buffer = ByteBuffer.allocate(20).put(tokenBytes)
        buffer.rewind()
        val tokenDigest = Array.ofDim[Byte](16)
        buffer.get(tokenDigest)
        if (!MessageDigest.isEqual(queryDigest, tokenDigest)) throw new InvalidNextTokenException
        buffer.getInt
      }).getOrElse(0)
    } catch { case e: IllegalArgumentException => throw new InvalidNextTokenException }

    val (items, itemsLength, itemsHasMore) = SelectParser.makeSelectEval(query).select(data, nextToken)

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
        {if (itemsHasMore)
          <NextToken>{
            DatatypeConverter.printBase64Binary(ByteBuffer.allocate(20)
                                                          .put(queryDigest)
                                                          .putInt(nextToken + itemsLength)
                                                          .array)
          }</NextToken>
        }
      </SelectResult>
      {responseMetaData}
    </SelectResponse>
  }

}
