import scala.collection._

/* 
Implementation of Vector Space Model used in text classification from Text Book
"Introduction to Information Retrieval" By Christopher D. Manning, Prabhakar Raghavan & Hinrich Schütze
*/
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

   def length={
     math.sqrt(termFreqs.foldLeft(0){(sum,n) => sum+n*n}) 
   }

   def intersectPos(other: TermVector): Seq[(Int,Int)] = {
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

class VectorSpace[DI] {

  var allTerms = new mutable.HashMap[String,mutable.HashSet[DI]]
  var allDocs = new mutable.HashMap[DI,TermVector]
  var nDocs = 0

  def add(docId: DI, doc: Seq[String]) = {
    if( !allDocs.contains(docId) ){
      allDocs += (docId->new TermVector(doc))
      nDocs += 1
      doc.foreach{ term=>
        if( !allTerms.contains(term) )
          allTerms += (term->new mutable.HashSet[DI])
        allTerms(term) += docId
      }
    }
  }

  def docFreq(term: String): Int = {
    if( !allTerms.contains(term) )
      return 0;
     
    allTerms(term).size
  }

  def docVector(docId: DI): TermVector = {
    allDocs(docId)
  }

  def idf(term: String) = {
    math.log(nDocs/docFreq(term))
  }

  def consine(one: DI, other: DI): Double = {
    if( !allDocs.contains(one) || !allDocs.contains(other)) 
      return 0.0

    val v1 = allDocs(one)
    val v2 = allDocs(other)

    val result = v1.intersectPos(v2).foldLeft(0.0) {case (sum,(i,j))=>
      if( v1.terms(i) != v2.terms(j) )
        throw new RuntimeException("v1.terms(i) != v2.term(j)")

      if( v1.termFreqs(i) == 0 )
        throw new RuntimeException("v1.termFreqs(i) == 0")

      if( v2.termFreqs(j) == 0 )
        throw new RuntimeException("v2.termFreqs(j) == 0")

      val tmp = idf(v1.terms(i))
      println("-- idf " + v1.terms(i) + " == " + tmp)

      sum+1.0*v1.termFreqs(i)*v2.termFreqs(j)*tmp*tmp/(v1.length*v2.length)
    }

    return result
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

     val result = classified.keys.map{ sample=>
       Tuple2(sample,vectorSpace.consine(test,sample))
       }.toList.sortBy(_._2).takeRight(k).groupBy{ 
       case (sample,score) => classified(sample)}.map{
       case (klass,samples) => (klass,samples.size)}.toSeq.sortBy(_._2).head._1

     return Some(result)
   }
}
