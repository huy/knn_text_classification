import org.scalatest.FunSuite
 
class VectorSpaceTest extends FunSuite {
 
  test("Figure 6.11") {
     val vs = new VectorSpace[Int]

     val doc1 = "car "*27 + "auto "*3 + "best "*14
     val doc2 = "car "*4  + "auto "*33 + "insurance "*33
     val doc3 = "car "*24 + "insurance "*29 + "best "*17

     Map(1->doc1,
         2->doc2,
         3->doc3).foreach{ case (docId,str)=>
       vs.vectorize(docId,str.split(" "))
     }

     expect(Array("auto","best","car").toList){vs.docIndex(1).terms.toList }
     expect(Array(3,14,27).toList){vs.docIndex(1).termFreqs.toList }
  }
 
}
