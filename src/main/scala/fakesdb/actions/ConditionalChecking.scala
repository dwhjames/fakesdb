package fakesdb.actions

import fakesdb._

trait ConditionalChecking {

  private val expectedNamePat = """Expected\.(?:\d+\.)?Name""".r
  private val expectedExistsPat = """Expected\.(?:\d+\.)?Exists""".r
  private val expectedValPat = """Expected\.(?:\d+\.)?Value""".r

  def checkConditionals(item: Item, params: Params): Unit = {
    for ((name, expect) <- discoverConditional(params))
      expect match {
        case None =>
          for (attr <- item.get(name))
            throw new SDBException(409, "ConditionalCheckFailed",
                                   "Conditional check failed. Attribute (%s) value exists.".format(name))
        case Some(expectedVal) =>
          item.get(name) match {
            case None =>
              throw new SDBException(404, "AttributeDoesNotExist", "Attribute (%s) does not exist".format(name))
            case Some(attr) =>
              if (attr.size > 1) {
                throw new SDBException(409, "MultiValuedAttribute",
                                       "Attribute (%s) is multi-valued. Conditional check can only be performed on a single-valued attribute".format(name))
              } else {
                // invariant: attributes have at least one value
                val actualVal = attr.iterator.next
                if (actualVal != expectedVal)
                throw new SDBException(409, "ConditionalCheckFailed",
                                       "Conditional check failed. Attribute (%s) value is (%s) but was expected (%s)".format(name, actualVal, expectedVal))
              }
          }
      }
  }

  private[fakesdb] def discoverConditional(params: Params): Option[Tuple2[String, Option[String]]] = {
    val nameKeys = params.keys collect { case k@expectedNamePat() => k}

    if (nameKeys.isEmpty) {
      None
    } else if (nameKeys.size > 1) {
      throw new SDBException(400, "MultipleExpectedNames", "Only one Expected.Name can be specified.")
    } else {
      val expectedName = params(nameKeys.head)

      val existsKeys = params.keys collect { case k@expectedExistsPat() => k }
      val valKeys = params.keys collect { case k@expectedValPat() => k }

      if (valKeys.isEmpty) {
        if (existsKeys.isEmpty) {
          throw new SDBException(400, "IncompleteExpectedExpression", "If Expected.Exists=true or unspecified, then Expected.Value has to be specified.")
        } else if (existsKeys.size > 1) {
          throw new SDBException(400, "MultipleExistsConditions", "Only one Exists condition can be specified.")
        } else {
          val shouldExist = params(existsKeys.head)
          if (shouldExist == "true") {
            throw new SDBException(400, "IncompleteExpectedExpression", "If Expected.Exists=true or unspecified, then Expected.Value has to be specified.")
          } else if (shouldExist == "false") {
            Some(expectedName -> None)
          } else {
            throw new InvalidParameterValue("Value (%s) for parameter Expected.Exists is invalid. Expected.Exists should be either true or false.".format(shouldExist))
          }
        }
      } else if (valKeys.size > 1) {
        throw new SDBException(400, "MultipleExpectedValues", "Only one Expected.Value can be specified.")
      } else {
        val expectedVal = params(valKeys.head)
        if (existsKeys.isEmpty) {
          Some(expectedName -> Some(expectedVal))
        } else if (existsKeys.size > 1) {
          throw new SDBException(400, "MultipleExistsConditions", "Only one Exists condition can be specified.")
        } else {
          val shouldExist = params(existsKeys.head)
          if (shouldExist == "true") {
            Some(expectedName -> Some(expectedVal))
          } else if (shouldExist == "false") {
            throw new SDBException(400, "ExistsAndExpectedValue", "Expected.Exists=false and Expected.Value cannot be specified together.")
          } else {
            throw new InvalidParameterValue("Value (%s) for parameter Expected.Exists is invalid. Expected.Exists should be either true or false.".format(shouldExist))
          }
        }
      }
    }
  }
}
