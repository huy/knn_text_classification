class NaiveBayesEnricher(var codeTable: CodeTable){
  var classifier = new NaiveBayes[String]

  codeTable.codeDefSeq.foreach {codeDef =>
    classifier.train(klass = codeDef.id, doc = codeDef.termSeq)
  }

  def enrich(unknown: CodeDef): Unit = {
    val (id,score) = classifier.apply(unknown.termSeq)
    codeTable.codeDef(id).merge(unknown)
    classifier.train(klass = id, doc = unknown.termSeq)
  }
}

class KNNEnricher(var codeTable: CodeTable, val k:Int = 2) {
  var corpus = new Corpus
  var classifier  = new KNN[String](corpus)

  codeTable.codeDefSeq.foreach {codeDef=>
    val docId = corpus.add(codeDef.termSeq)
    classifier.train(klass = codeDef.id, docId = docId)
  }

  def enrich(codeDef: CodeDef): Unit = {
    val docId = corpus.add(codeDef.termSeq)

    val id = classifier.apply(docId,k)
    codeTable.codeDef(id).merge(codeDef)
    classifier.train(klass = id, docId = docId)
  }

}
