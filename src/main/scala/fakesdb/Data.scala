package fakesdb

import fakesdb.actions.{SDBException, NumberItemAttributesExceededException,
                        EmptyAttributeNameException, InvalidParameterValue}
import java.util.regex.Pattern
import scala.collection.mutable

class Data {
  private val domains = new mutable.LinkedHashMap[String, Domain]
  private val domainNamePat = Pattern.compile("[a-zA-Z0-9_\\-\\.]{3,255}")
  def size: Int = domains.size
  def iterator: Iterator[Domain] = domains.valuesIterator
  def get(domainName: String): Option[Domain] = domains.get(domainName)
  def getOrCreate(domainName: String): Domain = {
    if (!domainNamePat.matcher(domainName).matches())
      throw new InvalidParameterValue("Value (%s) for parameter DomainName is invalid.".format(domainName))
    else
      domains.get(domainName).getOrElse {
        if (domains.size < Limits.MaxNumOfDomains) {
          val d = new Domain(domainName)
          domains.put(domainName, d)
          d
        } else
          throw new SDBException(409, "NumberDomainsExceeded", "The domain limit was exceeded.")
      }
  }
  def remove(domain: Domain): Unit = domains.remove(domain.name)
  def flush(): Unit = domains.clear()
}

class Domain(val name: String) {
  private val items = new mutable.LinkedHashMap[String, Item]
  def iterator: Iterator[Item] = items.valuesIterator
  def get(itemName: String): Option[Item] = items.get(itemName)
  def getOrCreate(itemName: String): Item = {
    InvalidParameterValue.failIfOver1024("Name", itemName);
    items.getOrElseUpdate(itemName, new Item(itemName))
  }
  def removeIfEmpty(item: Item): Unit = if (item.isEmpty) items.remove(item.name)
  def remove(item: Item): Unit = items.remove(item.name)
}

class Item(val name: String) {
  private val attributes = new mutable.LinkedHashMap[String, Attribute]
  def isEmpty: Boolean = attributes.isEmpty
  def iterator: Iterator[Attribute] = attributes.valuesIterator
  def get(attrName: String): Option[Attribute] = attributes.get(attrName)
  // this put overload is used in a lot of tests
  def put(attrName: String, value: String, replace: Boolean): Unit = put(attrName, List(value), replace)
  def put(attrName: String, values: Seq[String], replace: Boolean): Unit = {
    if (attrName == "") throw new EmptyAttributeNameException
    InvalidParameterValue.failIfOver1024("Name", attrName)
    // the limit is 256 (name,value) unique pairs, so make (name,value) pairs and then combine them 
    val existingPairs = (for ((name, attr) <- attributes; v <- attr.iterator) yield (name, v)).toSet
    val newPairs = values.map(value => (attrName, value)).toSet
    if ((existingPairs union newPairs).size > Limits.MaxNameValPairsPerItem)
      throw new NumberItemAttributesExceededException
    attributes.getOrElseUpdate(attrName, new Attribute(attrName)).put(values, replace)
  }
  def remove(attrName: String): Unit = attributes.remove(attrName)
  def remove(attrName: String, value: String): Unit =
    get(attrName) foreach { attr => attr.remove(value) ; removeIfEmpty(attr) }
  private def removeIfEmpty(attribute: Attribute): Unit =
    if (attribute.isEmpty) attributes.remove(attribute.name)
}

class Attribute(val name: String) {
  private val values = new mutable.LinkedHashSet[String]
  def iterator: Iterator[String] = values.iterator
  def isEmpty: Boolean = values.isEmpty
  def size: Int = values.size
  def remove(value: String): Unit = values.remove(value)
  def put(_values: Seq[String], replace: Boolean): Unit = {
    if (replace) values.clear()
    for (v <- _values) {
      InvalidParameterValue.failIfOver1024("Value", v);
      values += v
    }
  }
}
