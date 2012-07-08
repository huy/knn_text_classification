import scala.collection.mutable.HashMap
import com.codahale.logula.Logging

/**
Implements brute force K Nearest Neighbor Weighted by Proximity from text classiﬁcation algorithm from 
Text Book "Introduction to Information Retrieval" By Christopher D. Manning, Prabhakar Raghavan & Hinrich Schütze
**/
class KNN[C](
  proximity: (Int,Int)=>Double, 
  k: Int,
  info: Int=>String = _.toString) extends Logging{

  var classified = new HashMap[Int,C] 

  def train(sample: Int, klass: C) = {
    classified += sample->klass
  }

  def apply(test: Int) : Option[(C,Double)] = {
    val scorePerSample = classified.toSeq.map{case(sample,klass)=> Pair(klass,proximity(test, sample))}.
      filter{case(klass,score) => score > 0.0}.sortBy(_._2)
    
    log.debug("score per sample against %s:\n%s".format(info(test), scorePerSample.mkString(", ")))

    val topK = scorePerSample.takeRight(k)
   
    log.debug("top %d:\n%s".format(k, topK.mkString(", ")))
     
    if(topK.size == 0)
      return None

    val scorePerKlass = topK.groupBy{case (klass,score) => klass}.
      map{ case (klass,samples) => (klass,samples.foldLeft(0.0){ (sum,s) => sum + s._2 }) }.toSeq.sortBy(_._2)
    
    log.debug("score per class against %s:\n%s".format(info(test), scorePerKlass.mkString(", ")))
 
    Some(scorePerKlass.last)
  }
}
