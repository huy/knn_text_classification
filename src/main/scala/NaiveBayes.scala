import scala.collection._

/**
Implements Naive Bayes text classiﬁcation algorithm from Text Book
"Introduction to Information Retrieval" By Christopher D. Manning, Prabhakar Raghavan & Hinrich Schütze
**/
class NaiveBayes[C] {
   case class KlassInfo(var nDocs: Int, var nTerms: Int, var termFreq: mutable.Map[String,Int])

   var allKlassInfo = new mutable.HashMap[C,KlassInfo]
   var vocabulary = new mutable.HashSet[String]
   var nDocs = 0

   def train(klass: C, terms: List[String]) = {
     if( !allKlassInfo.contains(klass) ){
       allKlassInfo += (klass->new KlassInfo(0,0,new mutable.HashMap[String,Int]))
     }

     val klassInfo = allKlassInfo(klass)
     klassInfo.nDocs += 1
     nDocs += 1
     terms.foreach { t=>
       if( !klassInfo.termFreq.contains(t) )
         klassInfo.termFreq += (t->1)
       else
         klassInfo.termFreq(t) += 1
       klassInfo.nTerms += 1
       vocabulary += t
     }
   }

   private def probabilityTermInKlass(term: String, klass: C): Double={
     val klassInfo = allKlassInfo(klass)
     (klassInfo.termFreq(term) + 1.0)/(klassInfo.nTerms+vocabulary.size)
   }

   private def score(klass: C, terms: List[String]): Double = {
     val klassInfo = allKlassInfo(klass)
     val probabilityDocInKlass = (klassInfo.nDocs + 0.0)/nDocs

     val result = terms.foldLeft(math.log(probabilityDocInKlass)){ (sum,t) => 
        sum + math.log(probabilityTermInKlass(t,klass))
     }

     result
   }
}
