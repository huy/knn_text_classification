import scala.collection._

/**
Implements K Nearest Neighbor text classiﬁcation algorithm from Text Book
"Introduction to Information Retrieval" By Christopher D. Manning, Prabhakar Raghavan & Hinrich Schütze
**/
class KNN[C,DI](var corpus: Corpus[DI]) {
   var classified = new mutable.HashMap[DI,C] 

   def train(docId: DI,klass: C) = {
     if(corpus.allDocs.contains(docId)){
       if(classified.contains(docId))
          classified(docId) = klass

       classified += docId->klass
     }
   }

   def apply(test: DI, k: Int) : C = {
     if(!corpus.allDocs.contains(test))
       throw new RuntimeException("document %s is not in corpus".format(test))

     val result = classified.keys.map{ sample=>
       Tuple2(sample,corpus.consine(test,sample))
       }.toList.sortBy(_._2).takeRight(k).groupBy{ 
       case (sample,score) => classified(sample)}.map{
       case (klass,samples) => (klass,samples.size)}.toSeq.sortBy(_._2).head._1

     return result
   }
}
