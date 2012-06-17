import scala.io.Source
import scala.collection.mutable.{ListBuffer,HashMap}
import scala.util.matching.Regex

abstract class Enricher(var codeTable: CodeTable, val debug: Boolean = false) {
  def enrich(codeDef: CodeDef): Unit
}

class NaiveBayesEnricher(codeTable: CodeTable, debug: Boolean = false) extends Enricher(codeTable,debug){
  var classifier = new NaiveBayes[String](debug)

  codeTable.codeDefSeq.foreach {codeDef =>
    classifier.train(klass = codeDef.id, doc = codeDef.termSeq)
  }

  def enrich(codeDef: CodeDef): Unit = {
    classifier.apply(codeDef.termSeq, codeDef.id) match {
      case Some(klass) => {
        if(debug)
          println("--merge %s to %s".format(codeDef.id,klass))

        codeTable.codeDef(klass).merge(codeDef)
        classifier.train(klass = klass, doc = codeDef.termSeq)
      }
      case None => {
        if(debug)
          println("--found no def for %s".format(codeDef.id))
      }
    }
  }
}

class KNNEnricher(codeTable: CodeTable, val k:Int = 2, debug: Boolean = false) extends Enricher(codeTable,debug){
  var corpus = new Corpus

  var debugInfo = new HashMap[Int,String]
  def info(docId: Int) = debugInfo.getOrElse(docId,docId.toString)

  var classifier  = new KNN[String](proximity = corpus.cosine, debug = debug, info = info)

  codeTable.codeDefSeq.foreach {codeDef=>
    val docId = corpus.add(codeDef.termSeq)
    if(debug)
      debugInfo += (docId->codeDef.id)
    classifier.train(klass = codeDef.id, sample = docId)
  }

  def enrich(codeDef: CodeDef): Unit = {
    val docId = corpus.add(codeDef.termSeq)

    if(debug)
      debugInfo += (docId->codeDef.id)

    classifier.apply(test = docId, k = k) match {
      case Some(klass) => {
        if(debug)
          println("--merge %s to %s".format(info(docId),klass))

        codeTable.codeDef(klass).merge(codeDef)
        classifier.train(klass = klass, sample = docId)
      }
      case None => {
        if(debug)
          println("--found no def for %s".format(info(docId)))
      }
    }
  }
}

object Enricher {
  def usage() = {
    println("java -jar text_classification_2.9.2-1.0.min.jar --algo=nb|1nn|2nn|... --new-table=filename --existing-table=filename --result-table=filename [--code-id=id,...] [--debug]")
    System.exit(1)
  }

  def main(args: Array[String]) {

    var params = new HashMap[String,String]
    var debug = false

    val algoRegex = """^--algo=(\w+)""".r
    val newTabRegex = """^--new-table=(\S+)""".r
    val existingTabRegex = """^--existing-table=(\S+)""".r
    val resultTabRegex = """^--result-table=(\S+)""".r
    val codeIdRegex = """^--code-id=(\S+)""".r

    args.foreach{ a =>
      a match {
        case algoRegex(value) => params += ("algo"->value)
        case newTabRegex(value) => params += ("newTable"->value)
        case existingTabRegex(value) => params += ("existingTable"->value)
        case resultTabRegex(value) => params += ("resultTable"->value)
        case codeIdRegex(value) => params += ("codeId"->value)
        case "--debug" => debug = true
        case _ =>
      }
    }

    println("--params:\n%s".format(params))

    if(params.get("newTable") == None || params.get("existingTable") == None)
      usage()

    val existingTab = CodeTable.parseTextFile(params("existingTable"))
    val newTab = CodeTable.parseTextFile(params("newTable"))

    val knnRegex = """^(\d+)nn""".r
    var algo: Option[Enricher] = None

    params.get("algo") match {
      case Some("nb") => algo = Some(new NaiveBayesEnricher(codeTable = newTab, debug = debug))
      case Some(knnRegex(k)) => algo = Some(new KNNEnricher(codeTable = newTab, k = k.toInt, debug = debug))
      case _ => usage()
    }
    params.get("codeId") match {
      case Some(codeId) => {
        codeId.split(",").foreach { z =>
          existingTab.getCodeDef(z) match {
            case Some(codeDef) => algo.get.enrich(codeDef)
            case None => println("%s does not exists in %s".format(codeId,params("existingTable")))
          }
        }
      }
      case None => {
        existingTab.codeDefSeq.foreach {codeDef => algo.get.enrich(codeDef)}
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
