package fakesdb.actions

import scala.xml
import fakesdb._

class DeleteAttributes(data: Data) extends Action(data) with ConditionalChecking {

  def handle(params: Params): xml.Node = {
    val domain = parseDomain(params)
    val itemName = params.getOrElse("ItemName", throw new MissingItemNameException)
    InvalidParameterValue.failIfEmpty("Item", itemName)
    InvalidParameterValue.failIfOver1024("Item", itemName)
    domain.get(itemName) foreach { item =>
      checkConditionals(item, params)
      discoverAttributes(itemName, params).execDeleteOn(domain)
    }
    <DeleteAttributesResponse xmlns={namespace}>
      {responseMetaData}
    </DeleteAttributesResponse>
  }

  private[fakesdb] def discoverAttributes(itemName: String, params: Params): ItemUpdates = {
    val updates = new ItemUpdates
    updates.add(itemName)
    var i = 0
    var continue = false
    do {
      continue = i == 0 // allow for skipping 0, and thus counting from 1
      val attrName = params.get("Attribute.%d.Name".format(i))
      for (name <- attrName) {
        InvalidParameterValue.failIfEmpty("Name", name)
        InvalidParameterValue.failIfOver1024("Name", name)
      }
      val attrValue = params.get("Attribute.%d.Value".format(i))
      for (value <- attrValue) InvalidParameterValue.failIfOver1024("Value", value)

      attrName match {
        case None => attrValue foreach { value => throw new SDBException(400, "MissingParameter", "Attribute.%d.Name missing for Attribute.%d.Value='%s'.".format(i, i, value)) }
        case Some(name) => {
          continue = true
          attrValue match {
            case None => updates.add(itemName, name)
            case Some(value) => updates.add(itemName, name, value, false)
          }
        }
      }
      i += 1
    } while (continue)
    updates
  }
}
