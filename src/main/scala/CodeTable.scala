import scala.collection.mutable.{ListBuffer,HashMap}
import scala.util.matching.Regex
import scala.io.Source
import java.io.{File,PrintWriter}

class Origin(
  val ref: String, 
  val transferBy: String, 
  val confidence: Double){
  
  override def toString = {
    "%s %s %.2f".format(ref, transferBy, confidence)
  }

}

object Origin {
  def apply(
    ref: String, 
    transferBy: String, 
    confidence: Double) = new Origin(ref, transferBy, confidence) 

  def parseText(anotation: String): List[Origin] = {
    if(anotation.length > 0) {
      anotation.substring(1,anotation.length).split("<-").map{ z => 
        val arr = z.split(" ")
        if(arr.length != 3)
          throw new RuntimeException("anotation '%s' has wrong format".format(anotation))

        new Origin(arr(0),arr(1),arr(2).toDouble)
      }.toList  
    }else 
      Nil
  }
}

case class CodeInst(
  val desc: String, 
  val origin: List[Origin] = List.empty)

case class CodeDef(val id: String, val codeDesc: String = "", val desc: String) {

   val stopWords = Set(
    "a", "an", "and", "are", "as", "at", "be", "but", "by",
    "for", "if", "in", "into", "is", //"it", remove so not to be confused with IT
    "no", "not", "of", "on", "or", "such",
    "that", "the", "their", "then", "there", "these",
    "they", "this", "to", "was", "will", "with"
   )
  
   var instances = new ListBuffer[CodeInst]

   def merge(codeDef: CodeDef, origin: Option[Origin] = None) = {
     if(origin == None){
       instances += CodeInst(codeDef.desc)
       instances ++= codeDef.instances
     }
     else{
       instances += CodeInst(codeDef.desc, List(origin.get))
       instances ++= codeDef.instances.map { inst => 
         CodeInst(inst.desc, origin.get :: inst.origin) }
     }
   }

   private def stem(term: String) = {
     var stemmer = new Stemmer
     stemmer.add(term.toCharArray,term.size)
     stemmer.stem()
     stemmer.toString
   }

   def termSeq: Iterable[String] = {
     (desc.split("""\W""").toList ++: instances.map{z => z.desc.split("""\W""")}.flatten).
     map{z => if(z.toUpperCase == z) z else z.toLowerCase}.
     filterNot{z => z.isEmpty || stopWords.contains(z)}.
     map{z => stem(z)}
   }
}

class CodeTable(val name: String = "Unknown") {
  private var allCodeDefs = new HashMap[String,CodeDef]

  def add(codeDef: CodeDef) = allCodeDefs += (codeDef.id->codeDef)

  def codeDef(id: String): CodeDef = allCodeDefs(id)
  def getCodeDef(id: String): Option[CodeDef] = allCodeDefs.get(id)

  def codeDefSeq : Iterable[CodeDef] = allCodeDefs.values

  def size = allCodeDefs.size

  def toText: Iterable[String] = {
    allCodeDefs.values.toSeq.sortBy{ _.id }.map{ d => "%s\t%s".format(d.id, d.desc) +: 
      d.instances.map{ s => "-\t%s #%s".format(s.desc, s.origin.mkString("<-")) } }.flatten
  }

  def toTextFile(fileName: String) = {
    val p = new java.io.PrintWriter(new File(fileName))
    try { 
      toText.foreach{p.println}
    }
    finally { 
      p.close() 
    }
  }
}

object CodeTable {

  def parseText(lines: Iterator[String], name: String = "Unknown"): CodeTable = {
     val startDef = new Regex("""(\w+)\s+(.+)""")
     val continueDef = new Regex("""-\s+([^\#]+)(.*)""")
  
     var result = new CodeTable(name)
     var lineno = 1
     var currentDef: Option[CodeDef] = None
  
     lines.foreach { z =>
       z match {
         case startDef(code, desc) => { 
           if(currentDef != None)
             result.add(currentDef.get)
           currentDef = Some(CodeDef(id = code, desc = desc))
         }
         case continueDef(synonym, anotation) => {
           if(currentDef == None)
             throw new RuntimeException("line %d: '%s' has wrong format".format(lineno,z))
           else
             currentDef.get.instances += CodeInst(synonym,Origin.parseText(anotation))
         }  
         case _ => throw new RuntimeException("line %d: '%s' has wrong format".format(lineno,z))
       }  
       lineno += 1 
     }
     if(currentDef != None) result.add(currentDef.get)

     result
   }

   def parseTextFile(fileName: String): CodeTable = {
      parseText(Source.fromFile(fileName).getLines,fileName)     
   }
}
