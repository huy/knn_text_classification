import scala.collection._

/**
Implements Multinominal Naive Bayes text classiﬁcation algorithm from Text Book
"Introduction to Information Retrieval" By Christopher D. Manning, Prabhakar Raghavan & Hinrich Schütze
**/
class NaiveBayes[C] {
   case class KlassInfo(var nDocs: Int, var nTerms: Int, var termFreq: mutable.Map[String,Int])

   var allKlassInfo = new mutable.HashMap[C,KlassInfo]
   var vocabulary = new mutable.HashSet[String]
   var nDocs = 0

   def train(klass: C, doc: Seq[String]) = {
     if( !allKlassInfo.contains(klass) ){
       allKlassInfo += (klass->new KlassInfo(0,0,new mutable.HashMap[String,Int]))
     }

     val klassInfo = allKlassInfo(klass)
     klassInfo.nDocs += 1
     nDocs += 1
     doc.foreach { t=>
       if( !klassInfo.termFreq.contains(t) )
         klassInfo.termFreq += (t->1)
       else
         klassInfo.termFreq(t) += 1
       klassInfo.nTerms += 1
       vocabulary += t
     }
   }

   def info={
     println("vocabulary's size: " + vocabulary.size)
     println("nDocs: " + nDocs)
     allKlassInfo.foreach{ case (klass,info)=> 
       println("text length of " + klass  + ":" + info.nTerms)
       println("nDocs of " + klass  + ":" + info.nDocs)
       vocabulary.foreach { term =>
         val freq = if (info.termFreq.contains(term)) info.termFreq(term) else 0

         println("freq of term " + term + " in " + klass  + ":" + freq)
         println("P(t|c) - probability of term " + term + " in " + klass + ":" + 
                 probabilityTermInKlass(term,klass)) 
       }
     }
   }

   def apply(doc: Seq[String]): (C,Double) = {
     val str = doc.reduceLeft[String]{(acc,t) => acc+ " " + t }
     allKlassInfo.keys.map{ klass=> (klass,score(klass, doc))}.maxBy{_._2}
   }

   private def probabilityTermInKlass(term: String, klass: C): Double={
     val klassInfo = allKlassInfo(klass)
     val freq = if (klassInfo.termFreq.contains(term)) klassInfo.termFreq(term) else 0
     (freq + 1.0)/(klassInfo.nTerms+vocabulary.size)
   }

   private def score(klass: C, doc: Seq[String]): Double = {
     val klassInfo = allKlassInfo(klass)
     val probabilityDocInKlass = (klassInfo.nDocs + 0.0)/nDocs

     val result = doc.foldLeft(math.log(probabilityDocInKlass)){ (sum,t) => 
        sum + math.log(probabilityTermInKlass(t,klass))
     }

     result
   }
}
