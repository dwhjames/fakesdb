package fakesdb.actions

import scala.xml
import scala.collection.mutable
import fakesdb._

class DomainMetadata(data: Data) extends Action(data) {
  
  def handle(params: Params): xml.Node = {
    val domain = parseDomain(params)
    var itemCount = 0
    var itemNamesSizeBytes = 0
    var attributeNames = new mutable.HashSet[String]
    var attributeNamesSizeBytes = 0
    var attributeValueCount = 0
    var attributeValuesSizeBytes = 0
    for (item <- domain.iterator) {
      itemCount += 1
      itemNamesSizeBytes += item.name.getBytes.size
      for (attr <- item.iterator) {
        if (attributeNames.add(attr.name))
          attributeNamesSizeBytes += attr.name.getBytes.size
        for (value <- attr.iterator) {
          attributeValueCount += 1
          attributeValuesSizeBytes += value.getBytes.size
        }
      }
    }
    <DomainMetadataResponse xmlns={namespace}>
      <DomainMetadataResult>
        <ItemCount>{itemCount}</ItemCount>
        <ItemNamesSizeBytes>{itemNamesSizeBytes}</ItemNamesSizeBytes>
        <AttributeNameCount>{attributeNames.size}</AttributeNameCount>
        <AttributeNamesSizeBytes>{attributeNamesSizeBytes}</AttributeNamesSizeBytes>
        <AttributeValueCount>{attributeValueCount}</AttributeValueCount>
        <AttributeValuesSizeBytes>{attributeValuesSizeBytes}</AttributeValuesSizeBytes>
        <Timestamp>{System.currentTimeMillis()/1000}</Timestamp>
      </DomainMetadataResult>
      {responseMetaData}
    </DomainMetadataResponse>
  }

}
