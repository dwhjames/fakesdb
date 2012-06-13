package fakesdb

import fakesdb.actions.{SDBException, NumberItemAttributesExceededException,
                        EmptyAttributeNameException, InvalidParameterValue}
import java.util.regex.Pattern
import scala.collection.mutable.{LinkedHashSet, LinkedHashMap}

class Data {
  private val domains = new LinkedHashMap[String, Domain]()
  private val domainNamePat = Pattern.compile("[a-zA-Z0-9_\\-\\.]{3,255}")
  def getDomains(): Iterator[Domain] = domains.valuesIterator
  def getDomain(name: String): Domain =
    domains.getOrElse(name, throw new SDBException(400, "NoSuchDomain", "The specified domain does not exist."))
  def getOrCreateDomain(name: String): Domain = {
    if (!domainNamePat.matcher(name).matches())
      throw new InvalidParameterValue("Value (%s) for parameter DomainName is invalid.".format(name))
    else
      domains.get(name) match {
        case Some(d) => d
        case None =>
        if (domains.size < 250) {
          val d = new Domain(name)
          domains.put(name, d)
          d
        } else
          throw new SDBException(409, "NumberDomainsExceeded", "The domain limit was exceeded.")
      }
  }
  def deleteDomain(domain: Domain): Unit = domains.remove(domain.name)
  def flush(): Unit = domains.clear
}

class Domain(val name: String) {
  private val items = new LinkedHashMap[String, Item]()
  def getItems(): Iterator[Item] = items.valuesIterator
  def getItem(name: String): Option[Item] = items.get(name)
  def getOrCreateItem(name: String): Item = {
    InvalidParameterValue.failIfOver1024("Name", name);
    items.getOrElseUpdate(name, new Item(name))
  }
  def deleteIfEmpty(item: Item) = if (!item.getAttributes.hasNext) items.remove(item.name)
  def deleteItem(item: Item) = items.remove(item.name)
}

class Item(val name: String) {
  private val attributes = new LinkedHashMap[String, Attribute]()
  def getAttributes(): Iterator[Attribute] = attributes.valuesIterator
  def getAttribute(name: String): Option[Attribute] = attributes.get(name)
  def getOrCreateAttribute(name: String): Attribute = attributes.getOrElseUpdate(name, new Attribute(name))
  // this put overload is used in a lot of tests
  def put(name: String, value: String, replace: Boolean): Unit = {
    put(name, List(value), replace)
  }
  def put(name: String, values: Seq[String], replace: Boolean) {
    // the limit is 256 (name,value) unique pairs, so make (name,value) pairs and then combine them 
    val existingPairs = attributes.toList.flatMap((e) => { e._2.values.map((v) => (e._1, v)) })
    val newPairs = values.map((v) => (name, v))
    if ((existingPairs ++ newPairs).toSet.size > 256) {
      throw new NumberItemAttributesExceededException
    }
    if (name == "") {
      throw new EmptyAttributeNameException
    }
    InvalidParameterValue.failIfOver1024("Name", name);
    this.getOrCreateAttribute(name).put(values, replace)
  }
  def delete(name: String): Unit = attributes.remove(name)
  def delete(name: String, value: String): Unit = {
    getAttribute(name) match {
      case Some(a) => a.deleteValues(value) ; removeIfNoValues(a)
      case None =>
    }
  }
  private def removeIfNoValues(attribute: Attribute) = {
    if (attribute.empty) attributes.remove(attribute.name)
  }
}

class Attribute(val name: String) {
  private val _values = new LinkedHashSet[String]()
  def values(): Traversable[String] = _values
  def getValues(): Iterator[String] = _values.iterator
  def empty(): Boolean = values.size == 0
  def deleteValues(value: String) = {
    _values.remove(value)
  }
  def put(__values: Seq[String], replace: Boolean) = {
    if (replace) _values.clear
    __values.foreach((v) => {
      InvalidParameterValue.failIfOver1024("Value", v);
      _values += v
    })
  }
}
