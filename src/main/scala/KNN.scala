import scala.collection.mutable.HashMap

/**
Implements K Nearest Neighbor text classiﬁcation algorithm from Text Book
"Introduction to Information Retrieval" By Christopher D. Manning, Prabhakar Raghavan & Hinrich Schütze
**/
class KNN[C](distance:(Int,Int)=>Double, debug: Boolean = false) {
   var classified = new HashMap[Int,C] 

   def train(sample: Int, klass: C) = {
     classified += sample->klass
   }

   def apply(test: Int, k: Int) : Option[C] = {
     val scorePerSample = classified.keys.map{sample=> Tuple2(sample,distance(test, sample))
       }.toList.sortBy(_._2)
     
     if(debug)
       println("--score per sample against %d:\n%s".format(test, scorePerSample))

     val topK = scorePerSample.takeRight(k)
    
     if(debug)
       println("--topK:\n%s".format(topK))
      
     val scorePerKlass = topK.groupBy{case (sample,score) => classified(sample)
       }.map{case (klass,samples) => (klass,samples.foldLeft(0.0){(sum,s) => sum + s._2})}.toSeq.sortBy(_._2)
     
     if(debug)
       println("--score per class against %d:\n%s".format(test, scorePerKlass))
 
     if(scorePerKlass.forall{case(klass,score) => score == 0.0})
       None
     else
       Some(scorePerKlass.last._1)
   }
}
