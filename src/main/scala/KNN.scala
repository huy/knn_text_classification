import scala.collection._

/**
Implements K Nearest Neighbor text classiﬁcation algorithm from Text Book
"Introduction to Information Retrieval" By Christopher D. Manning, Prabhakar Raghavan & Hinrich Schütze
**/
class KNN[C](var corpus: Corpus) {
   var classified = new mutable.HashMap[Int,C] 

   def train(docId: Int, klass: C) = {
     if(corpus.allDocs.contains(docId)){
       if(classified.contains(docId))
          classified(docId) = klass

       classified += docId->klass
     }
   }

   def apply(docId: Int, k: Int) : C = {
     if(!corpus.allDocs.contains(docId))
       throw new RuntimeException("document %d is not in the corpus".format(docId))

     val result = classified.keys.map{sample=> Tuple2(sample,corpus.consine(docId, sample))
       }.toList.sortBy(_._2).takeRight(k).groupBy{case (sample,score) => classified(sample)
       }.map{case (klass,samples) => (klass,samples.size)}.toSeq.sortBy(_._2).head._1

     return result
   }
}
