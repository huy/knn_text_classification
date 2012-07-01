import scala.collection.mutable.{ListBuffer,HashMap,HashSet}

/* 
Implementation of TermVector that is needed by Vector Space Model inspired by Text Classification Chapter 
from Text Book "Introduction to Information Retrieval" By Christopher D. Manning, Prabhakar Raghavan & Hinrich Schütze,

The representation using two arrays, one of ordered terms and other of corresponding freq, is borrowed from Lucence 
*/
class TermVector(doc: Iterable[String]){

   val (terms,termFreqs) = process(doc)

   private def process(doc: Iterable[String]): (Array[String],Array[Int])= {
     var tmp = new HashMap[String,Int]
     doc.foreach { term => 
       if(!tmp.contains(term))
         tmp += (term->0)
       tmp(term) += 1 
     }
     var terms = new Array[String](tmp.size)
     var termFreqs = new Array[Int](tmp.size)
     var i = 0
     tmp.keys.toList.sorted.foreach { term=>
        terms(i) = term
        termFreqs(i) = tmp(term)
        i += 1
     }
     (terms,termFreqs)
   }

   private def indexOf(term: String): Int ={
     java.util.Arrays.binarySearch(terms.asInstanceOf[Array[Object]],term)
   }
  
   def tf(term: String): Int = {
     val pos = indexOf(term)     
     if(pos >= 0)
       termFreqs(pos)
     else
       0
   }

   def length(idf: String => Double): Double={
     math.sqrt(terms.foldLeft(0.0){ (sum,t) => 
      val idfTmp = idf(t)
      val tfTmp = termFreqs(indexOf(t))
      sum + (tfTmp*idfTmp)*(tfTmp*idfTmp) }) 
   }

   def intersectPos(other: TermVector): Iterable[(Int,Int)] = {
     var result = new ListBuffer[(Int,Int)]
     var i = 0
     terms.foreach { term=>
       val j = other.indexOf(term)
       if(j >= 0) 
         result += Pair(i,j)
       i += 1    
     }      
     result
   } 
}

/* 
Implementation of Document Corpus that is needed by Vector Space Model inspired by Text Classification Chapter 
from Text Book "Introduction to Information Retrieval" By Christopher D. Manning, Prabhakar Raghavan & Hinrich Schütze
*/
class Corpus {

  var allTerms = new HashMap[String, HashSet[Int]]
  var allDocs = new HashMap[Int, TermVector]
  var nDocs = 0

  def add(doc: Iterable[String]) : Int = {
    val docId = nDocs
    allDocs += (docId->new TermVector(doc))
    nDocs += 1
    doc.foreach{ term=>
      if( !allTerms.contains(term) )
        allTerms += (term->new HashSet[Int])
      allTerms(term) += docId
    }
    return docId
  }

  def docFreq(term: String): Int = {
    if( !allTerms.contains(term) )
      0
    else 
      allTerms(term).size
  }

  def docVector(docId: Int): TermVector = {
    allDocs(docId)
  }

  def idf(term: String): Double = {
    if (nDocs == docFreq(term)) 
      0.0
    else
      math.log(1.0*nDocs/docFreq(term))
  }

  def tfidf(docId: Int, term: String): Double = {
     if(!allDocs.contains(docId))
       0 
     else
       docVector(docId).tf(term)*idf(term)
  } 

  def cosine(one: Int, other: Int): Double = {
    if( !allDocs.contains(one) || !allDocs.contains(other)) 
      return 0.0

    val v1 = docVector(one)
    val v2 = docVector(other)
    
    var numerator = v1.intersectPos(v2).foldLeft(0.0) { case(sum,(i,j)) =>
      require(v1.termFreqs(i) > 0) 
      require(v2.termFreqs(j) > 0) 
      require(v1.terms(i) == v2.terms(j)) 

      val idfVal = idf(v1.terms(i))

      if(idfVal != 0.0) 
        sum + v1.termFreqs(i)*v2.termFreqs(j)*idfVal*idfVal
      else
        sum
    }

    val denominator = v1.length(idf)*v2.length(idf)

    return numerator/denominator
  }
}

