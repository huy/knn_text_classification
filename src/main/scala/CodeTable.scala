import scala.collection._
import scala.util.matching.Regex

case class CodeInst(val desc: String, val transfer: String = "", val confidence: Double = 1.0)

case class CodeDef(val id: String, val codeDesc: String = "", val desc: String) {
   var instances = new mutable.ListBuffer[CodeInst]

   def merge(codeDef: CodeDef) = {
     instances += CodeInst(codeDef.desc)
     instances ++: codeDef.instances
   }

   def termSeq: Seq[String] = {
     (desc.split("""\W""").toList ++: 
      instances.map{z=> z.desc.split("""\W""")}.flatten).filterNot{z=>z.isEmpty}.toList
   }
}

class NaiveBayesEnricher(var knowns: mutable.HashMap[String,CodeDef]){
  var classifier = new NaiveBayes[String]

  knowns.values.foreach {codeDef =>
    classifier.train(codeDef.id,codeDef.termSeq)
  }

  def enrich(unknown: CodeDef): Unit = {
    val (id,score) = classifier.apply(unknown.termSeq) 
    knowns(id).merge(unknown)
    classifier.train(klass=id, doc=unknown.termSeq)
  }
}

class KNNEnricher(var knowns: mutable.HashMap[String,CodeDef], val k:Int = 2) {
  var corpus = new Corpus[String]
  var classifier  = new KNN[String,String](corpus)
 
  knowns.values.foreach {codeDef=>
    corpus.add(docId = codeDef.id, doc = codeDef.termSeq)
    classifier.train(docId = codeDef.id, klass = codeDef.id)
  } 

  def enrich(unknown: CodeDef): Unit = {
    corpus.add(docId = unknown.id, doc = unknown.termSeq)

    val id = classifier.apply(unknown.id,k) 
    knowns(id).merge(unknown)
  }

}

object CodeTable {

  def parseText(lines: Iterator[String]): mutable.ListBuffer[CodeDef] = {
     var result = new mutable.ListBuffer[CodeDef]
     var lineno = 1
  
     val startDef = new Regex("""(\w+)\s+(.+)""")
     val continueDef = new Regex("""-\s+(.+)""")
     var currentDef: CodeDef = null
  
     lines.foreach { z =>
        z match {
          case startDef(code,desc) => { 
            if(currentDef != null)
              result += currentDef
            currentDef = CodeDef(id = code, desc = desc)
          }
          case continueDef(synonym) => {
            if(currentDef == null)
              throw new RuntimeException("line %d: '%s' has wrong format".format(lineno,z))
            else
              currentDef.instances += CodeInst(synonym)
          }  
          case _ => throw new RuntimeException("line %d: '%s' has wrong format".format(lineno,z))
        }  
        lineno += 1 
     }

     if(currentDef!=null) result+=currentDef

     result
  }
}
