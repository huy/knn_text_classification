import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
 
class KNNTest extends FunSuite with BeforeAndAfter {

  test("Knn for 2 classes inside and outside of interval 5..10"){
    val knn = new KNN[String](proximity = (a: Int, b: Int) => 1.0/(1.0+math.abs(a-b)), k = 3)

    knn.train(sample=5, klass= "inside")
    knn.train(sample=8, klass= "inside")
    knn.train(sample=10, klass= "inside")

    knn.train(sample=1, klass= "outside")
    knn.train(sample=4, klass= "outside")
    knn.train(sample=12, klass= "outside")
    knn.train(sample=15, klass= "outside")

    expect(7) {
      knn.classified.size
    }

    expect("inside"){
       knn.apply(test = 9).get._1
    }

    expect("outside"){
       knn.apply(test = 2).get._1
    }

    expect("outside"){
       knn.apply(test = 13).get._1
    }

  }

  test("Knn with cosine proximity"){
    val corpus = new Corpus
    val doc1 = "car "*27 + "auto "*3 + "best "*14
    val doc2 = "car "*4  + "auto "*33 + "insurance "*33 
    val doc3 = "car "*24 + "insurance "*29 + "best "*17

    val docId1 = corpus.add(doc1.split(" "))
    val docId2 = corpus.add(doc2.split(" "))
    val docId3 = corpus.add(doc3.split(" "))

    val knn = new KNN[String](proximity = corpus.cosine, k = 2)

    knn.train(sample = docId1, klass = "car")
    knn.train(sample = docId2, klass = "insurance")

    expect("insurance"){
       knn.apply(test = docId3).get._1
    }
  }
}
