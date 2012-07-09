import scala.io.Source
import scala.collection.mutable.{ListBuffer,HashMap}
import scala.util.matching.Regex
import com.codahale.logula.Logging

class Enricher(codeTable: CodeTable, k:Int = 3, threshold: Double = 0.5) extends Logging{
  var corpus = new Corpus

  var docsInfo = new HashMap[Int,String]
  def info(docId: Int) = docsInfo.getOrElse(docId,docId.toString)

  var classifier  = new KNN[String](proximity = corpus.cosine, k = k, info = info)

  codeTable.codeDefSeq.foreach { codeDef =>
    val docId = corpus.add(codeDef.termSeq)
    docsInfo += (docId->codeDef.id)
    classifier.train(klass = codeDef.id, sample = docId)
  }

  def enrich(codeDef: CodeDef): Unit = {
    val docId = corpus.add(codeDef.termSeq)

    docsInfo += (docId->codeDef.id)

    classifier.apply(docId) match {
      case Some((klass,score)) => {

        log.debug("found code '%s' for '%s'", klass, info(docId))

        if(score > threshold){
          log.debug("merge '%s' to code '%s'", info(docId),klass)

          codeTable.codeDef(klass).merge(
            codeDef,
            Some(Origin(codeTable.name.getOrElse("Unknown"),info(docId),"automatic",score))
          )

          classifier.train(klass = klass, sample = docId)
        }else{
          log.debug("reject merge '%s' to code '%s' because score %.2f < threshold %.2f",
              info(docId),klass,score,threshold)
        }
      }
      case None => {
        log.debug("found no code for '%s'", info(docId))
      }
    }
  }
}

object Enricher extends Logging{
  def usage() = {
    println(
"java -jar text_classification_2.9.2-1.0.min.jar | scala -jar text_classification_2.9.2-1.0.jar\n" +
"\t[--algo=1nn|2nn|...\n" +
"\t--new-table=filename\n" +
"\t--existing-table=filename\n" + 
"\t--result-table=filename\n" +
"\t[--threshold=0.5]\n" +
"\t[--code-id=id,...]\n" + 
"\t[--log=info|debug]"
    )
    System.exit(1)
  }

  def configureLog(level: String) = {
    import org.apache.log4j.Level
    val levels = Map("info"->Level.INFO, "debug"->Level.DEBUG)
    
    Logging.configure { log =>
      log.level = levels.getOrElse(level.toLowerCase, Level.INFO) // rootLogger level
      log.console.enabled = true
      log.console.threshold = log.level // same as rootLogger level
    }
  }

  def main(args: Array[String]) {

    var params = new HashMap[String,String]

    val paramsRegex = Map(
          "k"-> """^--algo=(\d+)nn$""".r,
          "newTable" -> """^--new-table=(\S+)$""".r,
          "existingTable" -> """^--existing-table=(\S+)$""".r,
          "resultTable" -> """^--result-table=(\S+)$""".r,
          "codeId" -> """^--code-id=(\S+)$""".r,
          "threshold"-> """^--threshold=0?(\.\d+)$""".r,
          "log"-> """^--log=(\S+)$""".r
        )

    args.foreach{ a =>
      paramsRegex.foreach { case (name,pattern) =>
        a match {
          case pattern(value) => params += (name->value)
          case _ =>
        }
      }
    }

    val logLevel = params.getOrElse("log","info")
    configureLog(logLevel)

    log.info("configure log with level: " + logLevel) 
    log.debug("params:\n%s".format(params.mkString(", ")))

    if(List("newTable", "existingTable", "k").exists{ name => params.get(name) == None })
      usage()

    val existingTab = CodeTable.parseTextFile(params("existingTable"))
    val newTab = CodeTable.parseTextFile(params("newTable"))

    val algo = new Enricher(
      codeTable = newTab, 
      k = params("k").toInt, 
      threshold = params.getOrElse("threshold",".5").toDouble)

    params.get("codeId") match {
      case Some(codeId) => {
        codeId.split(",").foreach { z =>
          existingTab.getCodeDef(z) match {
            case Some(codeDef) => algo.enrich(codeDef)
            case None => println("%s does not exists in %s".format(z,params("existingTable")))
          }
        }
      }
      case None => {
        existingTab.codeDefSeq.foreach {codeDef => algo.enrich(codeDef)}
      }
    }
    params.get("resultTable") match {
      case Some(fileName) => newTab.toTextFile(fileName)
      case None => {
        println("--result:")
        newTab.toText.foreach {println}
      }
    }
  }
}
