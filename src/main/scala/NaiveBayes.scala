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

   def train(klass: C, doc: Iterable[String]) = {
     if( !allKlassInfo.contains(klass) ){
       allKlassInfo += (klass->KlassInfo(0,0,new mutable.HashMap[String,Int]))
     }

     val klassInfo = allKlassInfo(klass)
     klassInfo.nDocs += 1
     nDocs += 1
     doc.foreach { term=>
       if( !klassInfo.termFreq.contains(term) )
         klassInfo.termFreq += (term->1)
       else
         klassInfo.termFreq(term) += 1
       klassInfo.nTerms += 1
       vocabulary += term
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
                 probabilityTermGivenKlass(term,klass)) 
       }
     }
   }

   def apply(doc: Iterable[String]): (C,Double) = {
     val str = doc.reduceLeft[String]{(acc,t) => acc+ " " + t }

     allKlassInfo.keys.map{ klass=> (klass,score(klass, doc))}.maxBy{_._2}
   }

   private def probabilityTermGivenKlass(term: String, klass: C): Double={
     val klassInfo = allKlassInfo(klass)
     val freq = klassInfo.termFreq.getOrElse(term,0)

     (freq + 1.0)/(klassInfo.nTerms+vocabulary.size)
   }

   private def score(klass: C, doc: Iterable[String]): Double = {
     val klassInfo = allKlassInfo(klass)
     val probabilityDocGivenKlass = (klassInfo.nDocs + 0.0)/nDocs

     val result = doc.foldLeft(math.log(probabilityDocGivenKlass)){(sum,t) => 
        sum + math.log(probabilityTermGivenKlass(t,klass))
     }

     result
   }
}
