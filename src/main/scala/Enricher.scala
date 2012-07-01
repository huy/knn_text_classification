import scala.io.Source
import scala.collection.mutable.{ListBuffer,HashMap}
import scala.util.matching.Regex

class Enricher(codeTable: CodeTable, k:Int = 3, threshold: Double = 0.5, debug: Boolean = false){
  var corpus = new Corpus

  var debugInfo = new HashMap[Int,String]
  def info(docId: Int) = debugInfo.getOrElse(docId,docId.toString)

  var classifier  = new KNN[String](proximity = corpus.cosine, k = k, debug = debug, info = info)

  codeTable.codeDefSeq.foreach { codeDef =>
    val docId = corpus.add(codeDef.termSeq)
    if(debug)
      debugInfo += (docId->codeDef.id)
    classifier.train(klass = codeDef.id, sample = docId)
  }

  def enrich(codeDef: CodeDef): Unit = {
    val docId = corpus.add(codeDef.termSeq)

    if(debug)
      debugInfo += (docId->codeDef.id)

    classifier.apply(docId) match {
      case Some((klass,score)) => {
        if(debug)
          println("---found code '%s' for '%s'".format(klass,info(docId)))

        if(score > threshold){
          if(debug)
            println("--merge '%s' to code '%s'".format(info(docId),klass))

          codeTable.codeDef(klass).merge(
            codeDef,
            Some(Origin(codeTable.name,info(docId),"automatic",score))
          )

          classifier.train(klass = klass, sample = docId)
        }else{
          if(debug)
            println("--reject merge '%s' to code '%s' because score %.2f < threshold %.2f".format(
              info(docId),klass,score,threshold))
        }
      }
      case None => {
        if(debug)
          println("--found no code for '%s'".format(info(docId)))
      }
    }
  }
}

object Enricher {
  def usage() = {
    println(
"java -jar text_classification_2.9.2-1.0.min.jar | scala -jar text_classification_2.9.2-1.0.jar\n" +
"\t[--algo=1nn|2nn|...\n" +
"\t--new-table=filename\n" +
"\t--existing-table=filename\n" + 
"\t--result-table=filename\n" +
"\t[--threshold=0.5]\n" +
"\t[--code-id=id,...]\n" + 
"\t[--debug]"
    )
    System.exit(1)
  }

  def main(args: Array[String]) {

    var params = new HashMap[String,String]

    val paramsRegex = Map(
          "k"-> """^--algo=(\d+)nn$""".r,
          "newTable" -> """^--new-table=(\S+)$""".r,
          "existingTable" -> """^--existing-table=(\S+)$""".r,
          "resultTable" -> """^--result-table=(\S+)$""".r,
          "codeId" -> """^--code-id=(\S+)$""".r,
          "threshold"-> """^--threshold=0?(\.\d+)$""".r
        )

    args.foreach{ a =>
      paramsRegex.foreach { case (name,pattern) =>
        a match {
          case pattern(value) => params += (name->value)
          case _ =>
        }
      }
    }
    val debug = args.exists { a => a == "--debug" }

    if(debug)
      println("--params:\n%s".format(params.mkString(", ")))

    if(List("newTable", "existingTable", "k").exists{ name => params.get(name) == None })
      usage()

    val existingTab = CodeTable.parseTextFile(params("existingTable"))
    val newTab = CodeTable.parseTextFile(params("newTable"))

    val algo = new Enricher(
      codeTable = newTab, 
      k = params("k").toInt, 
      threshold = params.getOrElse("threshold",".5").toDouble,
      debug = debug)

    params.get("codeId") match {
      case Some(codeId) => {
        codeId.split(",").foreach { z =>
          existingTab.getCodeDef(z) match {
            case Some(codeDef) => algo.enrich(codeDef)
            case None => println("%s does not exists in %s".format(codeId,params("existingTable")))
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
