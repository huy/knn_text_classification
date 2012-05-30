import scala.collection._

/**
Implements K Nearest Neighbor text classiﬁcation algorithm from Text Book
"Introduction to Information Retrieval" By Christopher D. Manning, Prabhakar Raghavan & Hinrich Schütze
**/

class SimpleDocIndex[DI] {

  class TermVector(doc: Seq[String]){

     private def process(doc: Seq[String]): (Array[String],Array[Int])= {
       var tmp = new mutable.HashMap[String,Int]
       doc.foreach { term => 
         if( !tmp.contains(term) )
           tmp += (term->0)
         tmp(term) += 1 
       }
       var terms = new Array[String](tmp.size)
       var termFreqs = new Array[Int](tmp.size)
       var i = 0
       tmp.keys.toList.sort{_<_}.foreach { term=>
          terms(i) = term
          termFreqs(i) = tmp(term)
          i += 1
       }
       (terms,termFreqs)
     }

     private def indexOf(term: String): Int ={
       java.util.Arrays.binarySearch(terms.asInstanceOf[Array[Object]],term)
     }

     def vectorLen={
       math.sqrt(termFreqs.foldLeft(0){(sum,n) => sum+n*n}) 
     }

     def intersect(other: TermVector): Seq[(Int,Int)] = {
       var result = new mutable.ListBuffer[(Int,Int)]
       var i = 0
       terms.foreach { term=>
         val j = other.indexOf(term)
         if(j >= 0) 
           result += Tuple2(i,j)
         i += 1    
       }      
       result
     } 

     var pair = process(doc)
     var terms = pair._1
     var termFreqs = pair._2
  }

  var termIndex = new mutable.HashMap[String,Int]
  var docIndex = new mutable.HashMap[DI,TermVector]
  var nDocs = 0

  def add(docId: DI, doc: Seq[String]) = {
    doc.foreach{ term=>
      if( !termIndex.contains(term) )
        termIndex += (term->0)
      termIndex(term) += 1
    }

    if( !docIndex.contains(docId) )
       docIndex += (docId->new TermVector(doc))
  }

  def docFreq(term: String): Int = {
    if( !termIndex.contains(term) )
      return 0;
     
    termIndex(term)
  }

  def score(docId1: DI, docId2: DI): Double = {
    if( !docIndex.contains(docId1) || !docIndex.contains(docId2)) 
      return 0.0 // ?should be None

    val v1 = docIndex(docId1)
    val v2 = docIndex(docId2)

    v1.intersect(v2).foldLeft(0.0) {case (sum,(i,j))=>
      val idf = math.log(nDocs/termIndex(v1.terms(i)))
      sum+1.0*v1.termFreqs(i)*v2.termFreqs(j)*idf*idf/(v1.vectorLen*v2.vectorLen)
    }
    
  }
}

class KNN[C,DI] {
}
