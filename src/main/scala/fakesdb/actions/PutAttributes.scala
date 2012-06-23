package fakesdb.actions

import scala.xml
import fakesdb._

class PutAttributes(data: Data) extends Action(data) with ConditionalChecking {

  def handle(params: Params): xml.Node = {
    val domain = parseDomain(params)
    val itemName = params.getOrElse("ItemName", throw new MissingItemNameException)
    InvalidParameterValue.failIfEmpty("Item", itemName)
    InvalidParameterValue.failIfOver1024("Item", itemName)
    val item = domain.getOrCreate(itemName)

    checkConditionals(item, params)

    discoverAttributes(itemName, params).update(domain)

    <PutAttributesResponse xmlns={namespace}>
      {responseMetaData}
    </PutAttributesResponse>
  }

  private[fakesdb] def discoverAttributes(itemName: String, params: Params): ItemUpdates = {
    val updates = new ItemUpdates
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

      val replace = try {
        params.get("Attribute.%d.Replace".format(i)).getOrElse("false").toBoolean
      } catch { case e: NumberFormatException => throw new InvalidParameterValue("Value (%s) for parameter Replace is invalid. The Replace flag should be either true or false.".format(params("Attribute.%d.Replace".format(i)))) }

      (attrName, attrValue) match {
        case (None, None) => {}
        case (None, Some(value)) =>
          throw new SDBException(400, "MissingParameter", "Attribute.%d.Name missing for Attribute.%d.Value='%s'.".format(i, i, value))
        case (Some(name), None) =>
          throw new SDBException(400, "MissingParameter", "Attribute.%d.Value missing for Attribute.%d.Name='%s'.".format(i, i, name))
        case (Some(name), Some(value)) => {
          updates.add(itemName, name, value, replace)
          continue = true
        }
      }
      i += 1
    } while (continue)
    if (updates.isEmpty) throw new SDBException(400, "MissingParameter", "The request must contain the parameter Attribute.Name")
    updates
  }
}
