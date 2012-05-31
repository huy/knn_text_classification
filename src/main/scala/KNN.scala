import scala.collection._

/* 
Implementation of Vector Space Model used in text classification from Text Book
"Introduction to Information Retrieval" By Christopher D. Manning, Prabhakar Raghavan & Hinrich Schütze
*/
class VectorSpace[DI] {

  class TermVector(doc: Seq[String]){

     private val pair = process(doc)

     def terms = pair._1
     def termFreqs = pair._2

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
       tmp.keys.toList.sortWith{_<_}.foreach { term=>
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
  }

  var allTerms = new mutable.HashMap[String,Int]
  var allDocs = new mutable.HashMap[DI,TermVector]
  var nDocs = 0

  def vectorize(docId: DI, doc: Seq[String]) = {
    doc.foreach{ term=>
      if( !allTerms.contains(term) )
        allTerms += (term->0)
      allTerms(term) += 1
    }

    if( !allDocs.contains(docId) )
       allDocs += (docId->new TermVector(doc))
  }

  def docFreq(term: String): Int = {
    if( !allTerms.contains(term) )
      return 0;
     
    allTerms(term)
  }

  def score(one: DI, other: DI): Option[Double] = {
    if( !allDocs.contains(one) || !allDocs.contains(other)) 
      return None

    val v1 = allDocs(one)
    val v2 = allDocs(other)

    val result = v1.intersect(v2).foldLeft(0.0) {case (sum,(i,j))=>
      val idf = math.log(nDocs/allTerms(v1.terms(i)))
      sum+1.0*v1.termFreqs(i)*v2.termFreqs(j)*idf*idf/(v1.vectorLen*v2.vectorLen)
    }

    return Some(result)
  }
}

/**
Implements K Nearest Neighbor text classiﬁcation algorithm from Text Book
"Introduction to Information Retrieval" By Christopher D. Manning, Prabhakar Raghavan & Hinrich Schütze
**/
class KNN[C,DI](var vectorSpace: VectorSpace[DI]) {
   var classified = new mutable.HashMap[DI,C] 

   def train(docId: DI,klass: C) = {
     if(vectorSpace.allDocs.contains(docId)){
       if(classified.contains(docId))
          classified(docId) = klass

       classified += docId->klass
     }
   }

   def apply(test: DI, k: Int) : Option[C] = {
     if(!vectorSpace.allDocs.contains(test))
       return None

     val result = classified.keys.map{sample=>
       Tuple2(sample,vectorSpace.score(test,sample))
     }.toList.sortBy(_._2).take(k).groupBy{
       case (sample,score)=> classified(sample)}.map{
       case (klass,samples) => (klass,samples.size)}.toList.sortBy(_._2).head._1

     return Some(result)
   }
}
