package fakesdb.actions

import scala.xml
import fakesdb._

class CreateDomain(data: Data) extends Action(data) {

  def handle(params: Params): xml.Node = {
    val domainName = params.getOrElse("DomainName", throw new MissingDomainNameException)
    if (domainName == "_flush") {
      data.flush() // The special one
    } else if (domainName == "_dump") {
      dump(domainName)
    } else {
      data.getOrCreate(domainName)
    }
    <CreateDomainResponse xmlns={namespace}>
      {responseMetaData}
    </CreateDomainResponse>
  }

  def dump(domainName: String) {
    for (d <- data.iterator) {
      println("Domain "+d.name)
      for (i <- d.iterator) {
        println("\tItem "+i.name)
        for (a <- i.iterator) {
          println("\t\t"+a.name+" = "+a.iterator.mkString(", "))
        }
      }
    }
  }
}
