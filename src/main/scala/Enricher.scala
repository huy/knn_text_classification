import scala.io._
import java.io._

abstract class Enricher(var codeTable: CodeTable) {
  def enrich(codeDef: CodeDef): Unit
}

class NaiveBayesEnricher(codeTable: CodeTable) extends Enricher(codeTable){
  var classifier = new NaiveBayes[String]

  codeTable.codeDefSeq.foreach {codeDef =>
    classifier.train(klass = codeDef.id, doc = codeDef.termSeq)
  }

  def enrich(codeDef: CodeDef): Unit = {
    val (id,score) = classifier.apply(codeDef.termSeq)
    codeTable.codeDef(id).merge(codeDef)
    classifier.train(klass = id, doc = codeDef.termSeq)
  }
}

class KNNEnricher(codeTable: CodeTable, val k:Int = 2) extends Enricher(codeTable){
  var corpus = new Corpus
  var classifier  = new KNN[String](distance = corpus.cosine)

  codeTable.codeDefSeq.foreach {codeDef=>
    val docId = corpus.add(codeDef.termSeq)
    classifier.train(klass = codeDef.id, sample = docId)
  }

  def enrich(codeDef: CodeDef): Unit = {
    val docId = corpus.add(codeDef.termSeq)

    val id = classifier.apply(test = docId, k = k)
    codeTable.codeDef(id).merge(codeDef)
    classifier.train(klass = id, sample = docId)
  }

}

object Enricher {
  val dataDir = "/Users/huy/github/text_classification/src/test/data"
  val sample = "%s/sample.txt".format(dataDir)
  val sampleNB = "%s/sample_nb.txt".format(dataDir)
  val sampleKNN = "%s/sample_knn.txt".format(dataDir)

  val tab = CodeTable.parseText(io.Source.fromFile(sample).getLines)

  def main(args: Array[String]) {

    var algo : Enricher = null

    if(args.exists{z => z == "--nb"}){
      println("--nb")
      algo =  new NaiveBayesEnricher(CodeTable.parseText(Source.fromFile(sample).getLines))
    }
    else{
      println("--knn")
      algo =  new KNNEnricher(CodeTable.parseText(Source.fromFile(sample).getLines),3)
    }

    CodeTable.parseText("""177     Administrator
-       office administrator
-       sales administrator
-       administration manager""".linesIterator).codeDefSeq.foreach{ test =>
      algo.enrich(test)
    }

    algo.codeTable.toText.foreach {println}

  }
}
