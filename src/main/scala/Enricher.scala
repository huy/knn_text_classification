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
    classifier.apply(codeDef.termSeq) match {
      case Some(klass) => {
        codeTable.codeDef(klass).merge(codeDef)
        classifier.train(klass = klass, doc = codeDef.termSeq)
      }
      case None => {
        codeTable.add(codeDef)
        classifier.train(klass = codeDef.id, doc = codeDef.termSeq)
      }
    }
  }
}

class KNNEnricher(codeTable: CodeTable, val k:Int = 2, debug: Boolean = false) extends Enricher(codeTable,debug){
  var corpus = new Corpus

  var debugInfo = new HashMap[Int,String]
  def info(docId: Int) = debugInfo.getOrElse(docId,docId.toString)

  var classifier  = new KNN[String](distance = corpus.cosine, debug = debug, info = info)

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
        codeTable.add(codeDef)

        if(debug)
          println("--found no def for %s".format(info(docId)))

        classifier.train(klass = codeDef.id, sample = docId)
      }
    }
  }
}

object Enricher {
  def usage() = {
    println("java -jar text_classification_2.9.2-1.0.min.jar --algo=[nb|knn[K]] --new-table=filename --existing-table=filename [--code-id=id] [--debug]")
    System.exit(1)
  }

  def main(args: Array[String]) {

    var params = new HashMap[String,String]
    var debug = false

    val algoRegex = """^--algo=(\w+)""".r
    val newTableRegex = """^--new-table=(\S+)""".r
    val existingTableRegex = """^--existing-table=(\S+)""".r
    val codeIdRegex = """^--code-id=(\w+)""".r

    args.foreach{ a =>
      a match {
        case algoRegex(value) => params += ("algo"->value)
        case newTableRegex(value) => params += ("newTable"->value)
        case existingTableRegex(value) => params += ("existingTable"->value)
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

    val knnRegex = """^knn(\d+)""".r
    var algo: Enricher = null

    params.get("algo") match {
      case Some("nb") => algo = new NaiveBayesEnricher(codeTable = newTab, debug = debug)
      case Some(knnRegex(k)) => algo = new KNNEnricher(codeTable = newTab, k = k.toInt, debug = debug)
      case _ => usage()
    }
    params.get("codeId") match {
      case Some(codeId) => {
        existingTab.getCodeDef(codeId) match {
          case Some(codeDef) => algo.enrich(codeDef)
          case None => println("%s does not exists in %s".format(codeId,params("existingTable")))
        }
      }
      case None => {
        existingTab.codeDefSeq.foreach {codeDef => algo.enrich(codeDef)}
      }
    }
    println("--result:")
    newTab.toText.foreach {println}
  }
}
