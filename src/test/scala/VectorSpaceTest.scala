import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
 
class VectorSpaceTest extends FunSuite with BeforeAndAfter {
 
  var vs = new VectorSpace[Int]

  before {
     val doc1 = "car "*27 + "auto "*3 + "best "*14
     val doc2 = "car "*4  + "auto "*33 + "insurance "*33
     val doc3 = "car "*24 + "insurance "*29 + "best "*17

     Map(1->doc1, 2->doc2, 3->doc3).foreach{ case (docId,str)=>
       vs.vectorize(docId,str.split(" "))
     }
  }

  test("term freqs") {
     expect(List("auto","best","car")){vs.allDocs(1).terms.toList }
     expect(List(3,14,27)){vs.allDocs(1).termFreqs.toList }
  }

  test("normalized term freqs") {
     expect(List(0.1,0.46,0.88).map{ntf=>math.round(ntf*vs.allDocs(1).vectorLen)}){
       vs.allDocs(1).termFreqs.toList
     }
  }
 
}
