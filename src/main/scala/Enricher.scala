import scala.io._
import java.io._
import scala.util.matching.Regex

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
  val sampleFile = "%s/sample.txt".format(dataDir)
  val testFile = "%s/test.txt".format(dataDir)

  val testTab = CodeTable.parseText(Source.fromFile(testFile).getLines)

  def main(args: Array[String]) {

    var algo : Enricher = null

    if(args.exists{z => z == "--nb"}){
      println("--nb")
      algo =  new NaiveBayesEnricher(CodeTable.parseText(Source.fromFile(sampleFile).getLines))
    }
    else{
      println("--knn")
      algo =  new KNNEnricher(CodeTable.parseText(Source.fromFile(sampleFile).getLines),3)
    }
    
    val testIdArg = new Regex("""--testId=(\w+)""")
    args.foreach { a=>
      a match {
        case testIdArg(testId) => {
          testTab.getCodeDef(testId) match {
            case Some(codeDef) => {
              algo.enrich(codeDef)
              algo.codeTable.toText.foreach {println}
            }
            case _ => println("%s does not exists in %s".format(testId,testFile))
          }
        }
        case _ =>
      }
    }
  }
}
