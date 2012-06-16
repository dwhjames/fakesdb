package fakesdb.actions

import scala.collection.mutable
import scala.xml
import fakesdb._

class GetAttributes(data: Data) extends Action(data) {

  def handle(params: Params): xml.Node = {
    val domain = parseDomain(params)
    val itemName = params.getOrElse("ItemName", throw new MissingItemNameException)
    InvalidParameterValue.failIfEmpty("Item", itemName)
    InvalidParameterValue.failIfOver1024("Item", itemName)
    val requested = discoverAttributes(params)
    <GetAttributesResponse xmlns={namespace}>
      <GetAttributesResult>
        {
          for (item <- domain.get(itemName).toList; (attr, value) <- filter(item, requested)) yield
            <Attribute><Name>{attr.name}</Name><Value>{value}</Value></Attribute>
        }
      </GetAttributesResult>
      {responseMetaData}
    </GetAttributesResponse>
  }

  protected def filter(item: Item, requested: Set[String]): Iterator[(Attribute, String)] = {
    val attrs = if (requested.isEmpty) item.iterator else item.iterator filter { attr => requested.contains(attr.name) }
    for (attr <- attrs; value <- attr.iterator) yield (attr, value)
  }

  protected def discoverAttributes(params: Params): Set[String] = {
    val requested = new mutable.HashSet[String]
    var i = 0
    var continue = false
    do {
      continue = i == 0 // allow for skipping 0, and thus counting from 1
      params.get("AttributeName.%d".format(i)) foreach { name =>
        InvalidParameterValue.failIfEmpty("Name", name)
        InvalidParameterValue.failIfOver1024("Name", name)
        requested += name
        continue = true
      }
      i += 1
    } while (continue)
    requested.toSet
  }
}
