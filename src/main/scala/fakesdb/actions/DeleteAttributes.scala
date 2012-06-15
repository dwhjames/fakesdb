package fakesdb.actions

import scala.collection.mutable.ListBuffer
import scala.xml
import fakesdb._

class DeleteAttributes(data: Data) extends Action(data) with ConditionalChecking {

  def handle(params: Params): xml.Node = {
    val domain = parseDomain(params)
    val itemName = params.getOrElse("ItemName", throw new MissingItemNameException)
    val item = domain.get(itemName) match {
      case Some(item) => {
        checkConditionals(item, params)
        doDelete(params, domain, item)
      }
      case _ =>
    }
    <DeleteAttributesResponse xmlns={namespace}>
      {responseMetaData}
    </DeleteAttributesResponse>
  }

  private def doDelete(params: Params, domain: Domain, item: Item) = {
    val destroy = discoverAttributes(params)
    if (destroy.isEmpty) {
      domain.remove(item)
    } else {
      for ((name, valOpt) <- destroy) valOpt match {
        case Some(value) => item.remove(name, value)
        case None => item.remove(name)
      }
      domain.removeIfEmpty(item)
    }
  }

  private def discoverAttributes(params: Params): List[(String, Option[String])] = {
    val attrs = new ListBuffer[(String, Option[String])]()
    var i = 0
    var stop = false
    while (!stop) {
      val attrName = params.get("Attribute."+i+".Name")
      val attrValue = params.get("Attribute."+i+".Value")
      if (attrName.isEmpty && attrValue.isEmpty) {
        if (i > 1) stop = true
      } else {
        attrs += Tuple2(attrName.get, attrValue)
      }
      i += 1
    }
    attrs.toList
  }

}
