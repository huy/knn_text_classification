import scala.collection._
import scala.util.matching.Regex
import scala.io.Source

case class CodeInst(val desc: String, val transfer: String = "", val confidence: Double = 1.0)

case class CodeDef(val id: String, val codeDesc: String = "", val desc: String) {

   var instances = new mutable.ListBuffer[CodeInst]

   def merge(codeDef: CodeDef) = {
     instances += CodeInst(codeDef.desc)
     instances ++= codeDef.instances
   }

   def termSeq: Iterable[String] = {
     (desc.split("""\W""").toList ++: instances.map{z=> z.desc.split("""\W""")}.flatten).
       filterNot{z=>z.isEmpty}.map{z=> 
          val up = z.toUpperCase
          if( up == z ) z else z.toLowerCase 
       }
   }
}

class CodeTable {
  private var allCodeDefs = new mutable.HashMap[String,CodeDef]

  def add(codeDef: CodeDef) = allCodeDefs += (codeDef.id->codeDef)

  def codeDef(id: String): CodeDef = allCodeDefs(id)
  def getCodeDef(id: String): Option[CodeDef] = allCodeDefs.get(id)

  def codeDefSeq : Iterable[CodeDef] = allCodeDefs.values

  def size = allCodeDefs.size

  def toText: Iterable[String] = {
    allCodeDefs.values.map{d => "%s\t%s".format(d.id,d.desc) +: 
      d.instances.map{s => "-\t%s".format(s.desc)}}.flatten
  }
}

object CodeTable {

  def parseText(lines: Iterator[String]): CodeTable = {
     val startDef = new Regex("""(\w+)\s+(.+)""")
     val continueDef = new Regex("""-\s+(.+)""")
  
     var result = new CodeTable
     var lineno = 1
     var currentDef: CodeDef = null
  
     lines.foreach { z =>
       z match {
         case startDef(code,desc) => { 
           if(currentDef != null)
             result.add(currentDef)
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
     if(currentDef!=null) result.add(currentDef)

     result
   }

   def parseTextFile(fileName: String): CodeTable = {
      parseText(Source.fromFile(fileName).getLines)     
   }
}
