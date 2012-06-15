package fakesdb.actions

import scala.collection.mutable.ListBuffer
import scala.xml
import fakesdb._

class GetAttributes(data: Data) extends Action(data) {

  def handle(params: Params): xml.Node = {
    val domain = parseDomain(params)
    val itemName = params.getOrElse("ItemName", throw new MissingItemNameException)
    val items = domain.get(itemName).toList
    val requested = discoverAttributes(params)
    <GetAttributesResponse xmlns={namespace}>
      <GetAttributesResult>
        {for (item <- items) yield
          {for (nv <- filter(item, requested)) yield
            <Attribute><Name>{nv._1.name}</Name><Value>{nv._2}</Value></Attribute>
          }
        }
      </GetAttributesResult>
      {responseMetaData}
    </GetAttributesResponse>
  }

  protected def filter(item: Item, requested: List[String]): Iterator[(Attribute, String)] = {
    val attrs = item.iterator.filter((a: Attribute) => requested.isEmpty || requested.contains(a.name))
    attrs.flatMap((a: Attribute) => a.iterator.map((v: String) => (a, v)))
  }

  protected def discoverAttributes(params: Params): List[String] = {
    val requested = new ListBuffer[String]()
    var i = 0
    var stop = false
    while (!stop) {
      val attrName = params.get("AttributeName."+i)
      if (attrName.isEmpty) {
        if (i > 1) stop = true
      } else {
        requested += attrName.get
      }
      i += 1
    }
    requested.toList
  }

}
