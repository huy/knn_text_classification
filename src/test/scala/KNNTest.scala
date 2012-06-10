import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
 
class KNNTest extends FunSuite with BeforeAndAfter {

  test("Knn"){
    val corpus = new Corpus
    val doc1 = "car "*27 + "auto "*3 + "best "*14
    val doc2 = "car "*4  + "auto "*33 + "insurance "*33
    val doc3 = "car "*24 + "insurance "*29 + "best "*17

    val docId1 = corpus.add(doc1.split(" "))
    val docId2 = corpus.add(doc2.split(" "))
    val docId3 = corpus.add(doc3.split(" "))

    val knn = new KNN[String](corpus)

    knn.train(docId1,"car")
    knn.train(docId2,"insurance")

    expect("car"){
       knn.apply(docId = docId3, k=2)
    }
  }
}
