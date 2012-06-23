package fakesdb.actions

import scala.collection.mutable
import fakesdb._

/** itemName -> [attrName -> AttributeUpdate] */
class ItemUpdates extends mutable.LinkedHashMap[String, mutable.LinkedHashMap[String, AttributeUpdate]] {
  def add(itemName: String, attrName: String, attrValue: String, replace: Boolean): Unit = {
    val attrs = getOrElseUpdate(itemName, new mutable.LinkedHashMap[String, AttributeUpdate])
    val attr = attrs.getOrElseUpdate(attrName, new AttributeUpdate(replace))
    attr.values += attrValue
  }

  def add(itemName: String, attrName: String): Unit = {
    val attrs = getOrElseUpdate(itemName, new mutable.LinkedHashMap[String, AttributeUpdate])
    val attr = attrs.getOrElseUpdate(attrName, new AttributeUpdate(false))
  }

  def add(itemName: String): Unit = {
    val attrs = getOrElseUpdate(itemName, new mutable.LinkedHashMap[String, AttributeUpdate])
  }

  def update(domain: Domain): Unit = {
    checkSize()
    foreach { case (itemName, attrs) =>
      if (attrs.size > Limits.MaxNameValPairsPerItem)
        throw new SDBException(409, "NumberSubmittedAttributesExceeded", "Too many attributes for item %s in a single call. Up to 256 attributes per call allowed.".format(itemName))
      else {
        val item = domain.getOrCreate(itemName)
        attrs.foreach { case (attrName, attrUpdate) =>
          item.put(attrName, attrUpdate.values.toSet, attrUpdate.replace)
        }
      }
    }
  }

  def delete(domain: Domain): Unit = {
    checkSize()
    foreach { case (itemName, attrs) => {
      val item = domain.get(itemName) match {
        case Some(item) => {
          if (attrs.isEmpty) {
            domain.remove(item)
          } else {
            attrs foreach { case (attrName, replace) => item.remove(attrName) }
            domain.removeIfEmpty(item)
          }
        }
        case _ =>
      }
    }}
  }

  private def checkSize() = {
    if (size > Limits.MaxNumOfSubmittedItems) {
       throw new NumberSubmittedItemsExceeded
    }
  }
}

class AttributeUpdate(val replace: Boolean) {
  val values = new mutable.HashSet[String]
}
