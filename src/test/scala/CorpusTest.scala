import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
 
class CorpusTest extends FunSuite with BeforeAndAfter {
 
  test("tf") {
    var corpus = new Corpus[Int]
    val doc = "car "*27 + "auto "*3 + "best "*14
    corpus.add(1,doc.split(" ")) 

    expect(List("auto","best","car")){corpus.docVector(1).terms.toList }
    expect(List(3,14,27)){corpus.docVector(1).termFreqs.toList }
  }

  test("length of term vector") {
    var corpus = new Corpus[Int]
    val doc = "car "*27 + "auto "*3 + "best "*14
    corpus.add(1,doc.split(" ")) 

    expect(math.sqrt(27*27+3*3+14*14)){
      corpus.docVector(1).length
    }
  }

  test("idf"){
    var corpus = new Corpus[Int]
    val doc1 = "car "*27 + "auto "*3 + "best "*14
    val doc2 = "car "*4  + "auto "*33 + "insurance "*33

    corpus.add(1,doc1.split(" ")) 
    corpus.add(2,doc2.split(" ")) 

    expect(2){
      corpus.nDocs
    }

    expect(List(2,1,2,1)){
      List("auto","best","car","insurance").map{term=>corpus.docFreq(term)}
    }

    expect(List(math.log(2.0/2),math.log(2.0/1),math.log(2.0/2),math.log(2.0/1))) {
      List("auto","best","car","insurance").map{term=>corpus.idf(term)}
    }
  }

  test("tfIdf"){
    var corpus = new Corpus[Int]
    val doc1 = "car "*27 + "auto "*3 + "best "*14
    val doc2 = "car "*4  + "auto "*33 + "insurance "*33

    corpus.add(1,doc1.split(" ")) 
    corpus.add(2,doc2.split(" ")) 


    expect(List(0,14*math.log(2.0/1),0)){
      List("auto","best","car").map{term=>
        corpus.tfidf(1,term)
      }
    }

    expect(List(0,0,33*math.log(2.0/1))){
      List("auto","car","insurance").map{term=>
        corpus.tfidf(2,term)
      }
    }
  }

  test("consine distance") {
    var corpus = new Corpus[Int]
    val doc1 = "car "*27 + "auto "*3 + "best "*14
    val doc2 = "car "*4  + "auto "*33 + "insurance "*33
    val doc3 = "car "*24 + "insurance "*29 + "best "*17
    corpus.add(1,doc1.split(" ")) 
    corpus.add(2,doc2.split(" ")) 
    corpus.add(3,doc3.split(" ")) 

    val v1 = corpus.docVector(1)
    val v2 = corpus.docVector(2)
    val v3 = corpus.docVector(3)

    expect(3){
      corpus.nDocs
    }

    expect(math.log(3.0/2)){
      corpus.idf("auto")
    }

    expect(List((0,0),(2,1))){ 
      v1.intersectPos(v2).toList
    }

    assert(0.0 != corpus.consine(1,2))
    
  }
}
