package fakesdb.actions

import scala.xml
import fakesdb._

class DomainMetadata(data: Data) extends Action(data) {
  
  def handle(params: Params): xml.Node = {
    def sum(list: List[Int]) = list.foldLeft(0)(_ + _)
    val allItems = data.iterator.flatMap(_.iterator).toList
    val allAttrs = allItems.flatMap(_.iterator.toList)
    val allValues = allAttrs.flatMap(_.iterator.toList)
    <DomainMetadataResponse xmlns={namespace}>
      <DomainMetadataResult>
        <ItemCount>{allItems.size}</ItemCount>
        <ItemNamesSizeBytes>{sum(allItems.map(_.name.size))}</ItemNamesSizeBytes>
        <AttributeNameCount>{allAttrs.toList.size}</AttributeNameCount>
        <AttributeNamesSizeBytes>{sum(allAttrs.map(_.name.size))}</AttributeNamesSizeBytes>
        <AttributeValueCount>{allValues.size}</AttributeValueCount>
        <AttributeValuesSizeBytes>{sum(allValues.map(_.size))}</AttributeValuesSizeBytes>
        <Timestamp>0</Timestamp>
      </DomainMetadataResult>
      {responseMetaData}
    </DomainMetadataResponse>
  }

}
