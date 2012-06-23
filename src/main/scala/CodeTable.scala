import scala.collection.mutable.{ListBuffer,HashMap}
import scala.util.matching.Regex
import scala.io.Source
import java.io.{File,PrintWriter}

case class CodeInst(val desc: String, val transfer: String = "", val confidence: Double = 1.0)

case class CodeDef(val id: String, val codeDesc: String = "", val desc: String) {

   val stopWords = Set(
    "a", "an", "and", "are", "as", "at", "be", "but", "by",
    "for", "if", "in", "into", "is", "it",
    "no", "not", "of", "on", "or", "such",
    "that", "the", "their", "then", "there", "these",
    "they", "this", "to", "was", "will", "with"
   )
  
   var instances = new ListBuffer[CodeInst]

   def merge(codeDef: CodeDef) = {
     instances += CodeInst(codeDef.desc)
     instances ++= codeDef.instances
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

class CodeTable {
  private var allCodeDefs = new HashMap[String,CodeDef]

  def add(codeDef: CodeDef) = allCodeDefs += (codeDef.id->codeDef)

  def codeDef(id: String): CodeDef = allCodeDefs(id)
  def getCodeDef(id: String): Option[CodeDef] = allCodeDefs.get(id)

  def codeDefSeq : Iterable[CodeDef] = allCodeDefs.values

  def size = allCodeDefs.size

  def toText: Iterable[String] = {
    allCodeDefs.values.toSeq.sortBy{ d => d.id }.map{ d => "%s\t%s".format(d.id,d.desc) +: 
      d.instances.map{ s => "-\t%s".format(s.desc) } }.flatten
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

  def parseText(lines: Iterator[String]): CodeTable = {
     val startDef = new Regex("""(\w+)\s+(.+)""")
     val continueDef = new Regex("""-\s+(.+)""")
  
     var result = new CodeTable
     var lineno = 1
     var currentDef: Option[CodeDef] = None
  
     lines.foreach { z =>
       z match {
         case startDef(code,desc) => { 
           if(currentDef != None)
             result.add(currentDef.get)
           currentDef = Some(CodeDef(id = code, desc = desc))
         }
         case continueDef(synonym) => {
           if(currentDef == None)
             throw new RuntimeException("line %d: '%s' has wrong format".format(lineno,z))
           else
             currentDef.get.instances += CodeInst(synonym)
         }  
         case _ => throw new RuntimeException("line %d: '%s' has wrong format".format(lineno,z))
       }  
       lineno += 1 
     }
     if(currentDef != None) result.add(currentDef.get)

     result
   }

   def parseTextFile(fileName: String): CodeTable = {
      parseText(Source.fromFile(fileName).getLines)     
   }
}
