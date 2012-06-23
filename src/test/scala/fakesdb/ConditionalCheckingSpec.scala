package fakesdb

import fakesdb.actions._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ConditionalCheckingSpec extends FlatSpec with ShouldMatchers {

  val obj = new AnyRef with ConditionalChecking

  "Conditional Checking" should "throw an exception for multiple expectations" in {
    var thrown = evaluating { obj.discoverConditional(Map("Expected.1.Name" -> "a", "Expected.2.Name" -> "b", "Expected.1.Exists" -> "true", "Expected.1.Value" -> "1")) } should produce [SDBException]
    thrown.xmlCode should equal ("MultipleExpectedNames")

    thrown = evaluating { obj.discoverConditional(Map("Expected.1.Name" -> "a", "Expected.1.Exists" -> "true", "Expected.2.Exists" -> "true", "Expected.1.Value" -> "1")) } should produce [SDBException]
    thrown.xmlCode should equal ("MultipleExistsConditions")

    thrown = evaluating { obj.discoverConditional(Map("Expected.1.Name" -> "a", "Expected.1.Exists" -> "false", "Expected.2.Exists" -> "false")) } should produce [SDBException]
    thrown.xmlCode should equal ("MultipleExistsConditions")

    thrown = evaluating { obj.discoverConditional(Map("Expected.1.Name" -> "a", "Expected.1.Exists" -> "true", "Expected.1.Value" -> "1", "Expected.2.Value" -> "2")) } should produce [SDBException]
    thrown.xmlCode should equal ("MultipleExpectedValues")
  }

  it should "throw an exception for an incomplete expected expression" in {
    var thrown = evaluating { obj.discoverConditional(Map("Expected.Name" -> "a")) } should produce [SDBException]
    thrown.xmlCode should equal ("IncompleteExpectedExpression")

    thrown = evaluating { obj.discoverConditional(Map("Expected.Name" -> "a", "Expected.Exists" -> "true")) } should produce [SDBException]
    thrown.xmlCode should equal ("IncompleteExpectedExpression")
  }

  it should "throw an exception for an exists condition that is not true or false" in {
    evaluating { obj.discoverConditional(Map("Expected.Name" -> "a", "Expected.Exists" -> "foo")) } should produce [InvalidParameterValue]

    evaluating { obj.discoverConditional(Map("Expected.Name" -> "a", "Expected.Exists" -> "foo", "Expected.Value" -> "1")) } should produce [InvalidParameterValue]
  }

  it should "throw an exception when both negative existence and an expected value are specified" in {
    val thrown = evaluating { obj.discoverConditional(Map("Expected.Name" -> "a", "Expected.Exists" -> "false", "Expected.Value" -> "1")) } should produce [SDBException]
    thrown.xmlCode should equal ("ExistsAndExpectedValue")
  }

  it should "throw an exception when an expected attribute is not present" in {
    val thrown = evaluating { obj.checkConditionals(new Item("myitem"), Map("Expected.Name" -> "a", "Expected.Exists" -> "true", "Expected.Value" -> "1")) } should produce [SDBException]
    thrown.xmlCode should equal ("AttributeDoesNotExist")
  }

  it should "throw an exception when an attribute unexpectedly exists" in {
    val item = new Item("myitem")
    item.put("a", "1", false)
    val thrown = evaluating { obj.checkConditionals(item, Map("Expected.Name" -> "a", "Expected.Exists" -> "false")) } should produce [SDBException]
    thrown.xmlCode should equal ("ConditionalCheckFailed")
  }

  it should "throw an exception for an expectation on a multi-valued attribute" in {
    val item = new Item("myitem")
    item.put("a", Set("1", "2"), false)
    val thrown = evaluating { obj.checkConditionals(item, Map("Expected.Name" -> "a", "Expected.Exists" -> "true", "Expected.Value" -> "1")) } should produce [SDBException]
    thrown.xmlCode should equal ("MultiValuedAttribute")
  }

  it should "throw an exception for a falisfied expectation" in {
    val item = new Item("myitem")
    item.put("a", "1", false)
    val thrown = evaluating { obj.checkConditionals(item, Map("Expected.Name" -> "a", "Expected.Exists" -> "true", "Expected.Value" -> "2")) } should produce [SDBException]
    thrown.xmlCode should equal ("ConditionalCheckFailed")
  }

  it should "succeed when an attribute does not exist, as expected" in {
    val item = new Item("myitem")
    item.put("a", "1", false)
    obj.checkConditionals(item, Map("Expected.Name" -> "b", "Expected.Exists" -> "false"))
  }

  it should "succeed when the expected value for an attribute is found" in {
    val item = new Item("myitem")
    item.put("a", "1", false)
    item.put("b", "2", false)
    obj.checkConditionals(item, Map("Expected.Name" -> "a", "Expected.Value" -> "1"))
    obj.checkConditionals(item, Map("Expected.Name" -> "b", "Expected.Exists" -> "true", "Expected.Value" -> "2"))
  }
}
