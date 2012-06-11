import scala.io._
import java.io._

class NaiveBayesEnricher(var codeTable: CodeTable){
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

class KNNEnricher(var codeTable: CodeTable, val k:Int = 2) {
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
  val newTabFileBefore = "%s/sample.txt".format(dataDir)
  val newTabFileByNB = "%s/sample_nb.txt".format(dataDir)
  val newTabFileByKNN = "%s/sample_knn.txt".format(dataDir)
  val existingTabFile = "%s/test.txt".format(dataDir)

  def main(args: Array[String]) {
    var tabNB = CodeTable.parseText(io.Source.fromFile(newTabFileBefore).getLines)
    var tabKNN = CodeTable.parseText(io.Source.fromFile(newTabFileBefore).getLines)

    val existingTab = CodeTable.parseText(io.Source.fromFile(existingTabFile).getLines)

    var nb = new NaiveBayesEnricher(tabNB)   
    var knn = new KNNEnricher(tabKNN,3)   

    existingTab.codeDefSeq.foreach { test =>
      nb.enrich(test) 
      knn.enrich(test)
    }

    val outNB = new PrintWriter(newTabFileByNB)
    tabNB.toText.foreach {outNB.println}
    outNB.close

    val outKNN = new PrintWriter(newTabFileByKNN)
    tabKNN.toText.foreach {outKNN.println}
    outKNN.close
  }
}
