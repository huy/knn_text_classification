import scala.collection._

/**
Implements K Nearest Neighbor text classiﬁcation algorithm from Text Book
"Introduction to Information Retrieval" By Christopher D. Manning, Prabhakar Raghavan & Hinrich Schütze
**/
class KNN[C](distance:(Int,Int)=>Double) {
   var classified = new mutable.HashMap[Int,C] 

   def train(sample: Int, klass: C) = {
     classified += sample->klass
   }

   def apply(test: Int, k: Int) : C = {
     val result = classified.keys.map{sample=> Tuple2(sample,distance(test, sample))
       }.toList.sortBy(_._2).takeRight(k).groupBy{case (sample,score) => classified(sample)
       }.map{case (klass,samples) => (klass,samples.size)}.toSeq.sortBy(_._2).head._1

     return result
   }
}
