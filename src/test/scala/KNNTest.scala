import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
 
class KNNTest extends FunSuite with BeforeAndAfter {

  test("Knn"){
    val corpus = new Corpus[Int]
    val doc1 = "car "*27 + "auto "*3 + "best "*14
    val doc2 = "car "*4  + "auto "*33 + "insurance "*33
    val doc3 = "car "*24 + "insurance "*29 + "best "*17
    Map(1->doc1,2->doc2,3->doc3).foreach {case(docId,doc) =>
      corpus.add(docId,doc.split(" "))
    }

    val knn = new KNN[String,Int](corpus)

    knn.train(1,"car")
    knn.train(2,"insurance")

    expect("car"){
       knn.apply(3, 2)
    }
  }
}
