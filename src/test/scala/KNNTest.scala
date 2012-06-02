import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
 
class VectorSpaceTest extends FunSuite with BeforeAndAfter {
 
  test("term freqs") {
    var vs = new VectorSpace[Int]
    val doc = "car "*27 + "auto "*3 + "best "*14
    vs.add(1,doc.split(" ")) 

    expect(List("auto","best","car")){vs.docVector(1).terms.toList }
    expect(List(3,14,27)){vs.docVector(1).termFreqs.toList }
  }

  test("vector length") {
    var vs = new VectorSpace[Int]
    val doc = "car "*27 + "auto "*3 + "best "*14
    vs.add(1,doc.split(" ")) 

    expect(math.sqrt(27*27+3*3+14*14)){
      vs.docVector(1).length
    }
  }

  test("doc freqs and idf"){
    var vs = new VectorSpace[Int]
    val doc1 = "car "*27 + "auto "*3 + "best "*14
    val doc2 = "car "*4  + "auto "*33 + "insurance "*33

    vs.add(1,doc1.split(" ")) 
    vs.add(2,doc2.split(" ")) 

    expect(2){
      vs.nDocs
    }

    expect(2){
      vs.docFreq("car")
    }

    expect(1){
      vs.docFreq("insurance")
    }

    expect(math.log(2/2)) {
      vs.idf("car")
    }

    expect(math.log(2/1)) {
      vs.idf("best")
    }

  }

  test("consine distance") {
    var vs = new VectorSpace[Int]
    val doc1 = "car "*27 + "auto "*3 + "best "*14
    val doc2 = "car "*4  + "auto "*33 + "insurance "*33
    vs.add(1,doc1.split(" ")) 
    vs.add(2,doc2.split(" ")) 

    val v1 = vs.docVector(1)
    val v2 = vs.docVector(2)

    expect(List((0,0),(2,1))){ 
      v1.intersectPos(v2).toList
    }

    expect(0.0) {
      vs.consine(1,2)
    }
    
  }
}

class KNNTest extends FunSuite with BeforeAndAfter {

  test("Knn"){
    val vs = new VectorSpace[Int]
    val doc1 = "car "*27 + "auto "*3 + "best "*14
    val doc2 = "car "*4  + "auto "*33 + "insurance "*33
    val doc3 = "car "*24 + "insurance "*29 + "best "*17
    Map(1->doc1,2->doc2,3->doc3).foreach {case(docId,doc) =>
      vs.add(docId,doc.split(" "))
    }

    val knn = new KNN[String,Int](vs)

    knn.train(1,"car")
    knn.train(2,"insurance")

    //expect(Some("car")){
    //   knn.apply(3, 2)
    //}
  }
}
