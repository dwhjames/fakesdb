package fakesdb

import fakesdb.actions._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.BeforeAndAfter

class ItemUpdatesSpec extends FlatSpec with ShouldMatchers with BeforeAndAfter {

  val data = new Data
  val domain = data.getOrCreate("mydom")
  var item: Item = _

  before {
    item = domain.getOrCreate("myitem")
  }
  
  after {
    domain.remove(item)
  }

  "ItemUpdates" should "take just an item name" in {
    val upd = new ItemUpdates
    upd should be ('empty)
    upd.add("myitem")
    upd.size should be (1)
    upd("myitem") should be ('empty)
  }
  
  it should "take multiple item names" in {
    val upd = new ItemUpdates
    upd.add("item1")
    upd.add("item2")
    upd.add("item3")
    upd.size should be (3)
    upd("item1") should be ('empty)
    upd("item2") should be ('empty)
    upd("item3") should be ('empty)
  }

  it should "take an item name and just an attribute name" in {
    val upd = new ItemUpdates
    upd.add("myitem", "attr1")
    upd.size should be (1)
    val attrUpd = upd("myitem")
    attrUpd.size should be (1)
    attrUpd("attr1").values should be ('empty)
  }
  
  it should "ensure adding an item is idempotent" in {
    val upd = new ItemUpdates
    upd.add("myitem") // test idempotence
    upd.add("myitem", "attr1")
    upd.size should be (1)
    val attrUpd = upd("myitem")
    attrUpd.size should be (1)
    attrUpd("attr1").values should be ('empty)
  }

  it should "take multiple attribute names" in {
    val upd = new ItemUpdates
    upd.add("myitem", "attr1")
    upd.add("myitem", "attr2")
    upd.add("myitem", "attr3")
    upd.size should be (1)
    val attrUpd = upd("myitem")
    attrUpd.size should be (3)
    attrUpd("attr1").values should be ('empty)
    attrUpd("attr2").values should be ('empty)
    attrUpd("attr3").values should be ('empty)
  }

  it should "take an item name, an attribute name, a value, and a replace flag" in {
    val upd = new ItemUpdates
    upd.add("myitem", "attr1", "val1", true)
    upd.size should be (1)
    val attrUpd = upd("myitem")
    attrUpd.size should be (1)
    attrUpd("attr1").values should equal (Set("val1"))
    attrUpd("attr1").replace should be (true)
  }

  it should "ensure adding an attribute name is idempotent" in {
    val upd = new ItemUpdates
    upd.add("myitem", "attr1")
    upd.add("myitem", "attr1", "val1", true)
    upd.size should be (1)
    val attrUpd = upd("myitem")
    attrUpd.size should be (1)
    attrUpd("attr1").values should equal (Set("val1"))
    attrUpd("attr1").replace should be (true)
  }

  it should "ensure name-val pairs are unique" in {
    val upd = new ItemUpdates
    upd.add("myitem", "attr1", "val1", false)
    upd.add("myitem", "attr1", "val1", false)
    upd.size should be (1)
    val attrUpd = upd("myitem")
    attrUpd.size should be (1)
    attrUpd("attr1").values should equal (Set("val1"))
    attrUpd("attr1").replace should be (false)
  }

  it should "take multiple attribute values" in {
    val upd = new ItemUpdates
    upd.add("myitem", "attr1", "val1", false)
    upd.add("myitem", "attr1", "val2", false)
    upd.size should be (1)
    val attrUpd = upd("myitem")
    attrUpd.size should be (1)
    attrUpd("attr1").values should equal (Set("val1", "val2"))
    attrUpd("attr1").replace should be (false)
  }

  it should "take a disjunction of the replace flags" in {
    val upd = new ItemUpdates
    upd.add("myitem", "attr1", "val1", false)
    upd.add("myitem", "attr1", "val2", true)
    upd.size should be (1)
    val attrUpd = upd("myitem")
    attrUpd.size should be (1)
    attrUpd("attr1").values should equal (Set("val1", "val2"))
    attrUpd("attr1").replace should be (true)
  }

  it should "throw an exception for too many updated attributes" in {
    val upd = new ItemUpdates
    for (i <- 0 to Limits.MaxNameValPairsPerItem) {
      upd.add("myitem", "attr%d".format(i), "val%d".format(i), true)
    }
    val thrown = evaluating { upd.execUpdateOn(domain) } should produce [SDBException]
    thrown.xmlCode should equal ("NumberSubmittedAttributesExceeded")
  }

  it should "add new attributes to an item" in {
    val upd = new ItemUpdates
    upd.add("myitem", "attr1", "val1", false)
    upd.add("myitem", "attr2", "val2", false)
    upd.execUpdateOn(domain)
    item.get("attr1").get.iterator.toSet should equal (Set("val1"))
    item.get("attr2").get.iterator.toSet should equal (Set("val2"))
  }

  it should "add a value to an existing attribute in a item" in {
    item.put("attr1", "val1", false)
    val upd = new ItemUpdates
    upd.add("myitem", "attr1", "val2", false)
    upd.execUpdateOn(domain)
    item.get("attr1").get.iterator.toSet should equal (Set("val1", "val2"))
  }

  it should "replace an attribute value in a item" in {
    item.put("attr1", "val1", false)
    val upd = new ItemUpdates
    upd.add("myitem", "attr1", "val2", true)
    upd.execUpdateOn(domain)
    item.get("attr1").get.iterator.toSet should equal (Set("val2"))
  }

  it should "delete an item from a domain" in {
    val upd = new ItemUpdates
    upd.add("myitem")
    upd.execDeleteOn(domain)
    domain.iterator.size should be (0)
  }

  it should "delete an attribute from an item" in {
    item.put("attr1", "val1", false)
    item.put("attr2", "val2", false)
    val upd = new ItemUpdates
    upd.add("myitem", "attr1")
    upd.execDeleteOn(domain)
    item.iterator.size should be (1)
    item.get("attr2").get
  }

  it should "remove items that become empty after deleting an attribute" in {
    item.put("attr1", "val1", false)
    val upd = new ItemUpdates
    upd.add("myitem", "attr1")
    upd.execDeleteOn(domain)
    domain.iterator.size should be (0)
  }

  it should "delete a value from an attribute" in {
    item.put("attr1", Set("val1", "val2"), false)
    val upd = new ItemUpdates
    upd.add("myitem", "attr1", "val1", false)
    upd.execDeleteOn(domain)
    item.iterator.size should be (1)
    item.get("attr1").get.iterator.toSet should equal (Set("val2"))
  }

  it should "remove items that become empty after deleting an attribute value" in {
    item.put("attr1", "val1", false)
    val upd = new ItemUpdates
    upd.add("myitem", "attr1", "val1", false)
    upd.execDeleteOn(domain)
    domain.iterator.size should be (0)
  }
}
