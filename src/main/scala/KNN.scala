import scala.collection.mutable.HashMap

/**
Implements brute force K Nearest Neighbor Weighted by Proximity from text classiﬁcation algorithm from 
Text Book "Introduction to Information Retrieval" By Christopher D. Manning, Prabhakar Raghavan & Hinrich Schütze
**/
class KNN[C](
  proximity: (Int,Int)=>Double, 
  k: Int,
  debug: Boolean = false, 
  info: Int=>String = _.toString) {

  var classified = new HashMap[Int,C] 

  def train(sample: Int, klass: C) = {
    classified += sample->klass
  }

  def apply(test: Int) : Option[(C,Double)] = {
    val scorePerSample = classified.toSeq.map{case(sample,klass)=> Pair(klass,proximity(test, sample))}.
      sortBy(_._2).filter{case(klass,score) => score > 0.0}.sortBy(_._2)
    
    if(debug)
      println("--score per sample against %s:\n%s".format(info(test), scorePerSample.mkString(", ")))

    val topK = scorePerSample.takeRight(k)
   
    if(debug)
      println("--top %d:\n%s".format(k, topK.mkString(", ")))
     
    val scorePerKlass = topK.groupBy{case (klass,score) => klass}.
      map{ case (klass,samples) => (klass,samples.foldLeft(0.0){ (sum,s) => sum + s._2 }) }.toSeq.sortBy(_._2)
    
    if(debug)
      println("--score per class against %s:\n%s".format(info(test), scorePerKlass.mkString(", ")))
 
    if(scorePerKlass.size == 0)
      None
    else
      Some(scorePerKlass.last)
  }
}
