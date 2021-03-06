import scala.collection.mutable.{ListBuffer,HashMap}
import scala.util.matching.Regex
import scala.io.Source
import java.io.{File,PrintWriter}

class Origin(
  val tableName: String, 
  val codeId: String,
  val transferBy: String, 
  val confidence: Double){
  
  override def toString = {
    "%s %s %s %.2f".format(tableName, codeId, transferBy, confidence)
  }
}

object Origin {
  def apply(
    tableName: String, 
    codeId: String,
    transferBy: String, 
    confidence: Double) = new Origin(tableName, codeId, transferBy, confidence) 

  def parseText(annotation: String): List[Origin] = {
    if(annotation.length > 0) {
      annotation.substring(1,annotation.length).split("<-").map{ z => 
        val arr = z.split(" ")
        if(arr.length != 4)
          throw new RuntimeException("Error annotation '%s' has wrong format".format(annotation))

        new Origin(
          tableName = arr(0), 
          codeId = arr(1), 
          transferBy = arr(2), 
          confidence = 
            try { 
              arr(3).toDouble 
            }catch { 
              case ex: Exception => throw new RuntimeException("Error confidence score '%s' has wrong format".format(arr(3)), ex) 
            })

      }.toList  
    }else 
      List.empty
  }
}

case class CodeInst(
  val desc: String, 
  val origin: List[Origin] = List.empty)

case class CodeDef(val id: String, val desc: String) {

   val stopWords = Set(
    "a", "an", "and", "are", "as", "at", "be", "but", "by",
    "for", "if", "in", "into", "is", //"it", remove so not to be confused with IT
    "no", "not", "of", "on", "or", "such",
    "that", "the", "their", "then", "there", "these",
    "they", "this", "to", "was", "will", "with"
   )
  
   var instances = new ListBuffer[CodeInst]

   def merge(codeDef: CodeDef, origin: Option[Origin] = None) = {
     origin match {
       case Some(x) => {
         instances += CodeInst(codeDef.desc, List(x))
         instances ++= codeDef.instances.map { inst => 
           CodeInst(inst.desc, x :: inst.origin) }
       }
       case None => {
         instances += CodeInst(codeDef.desc)
         instances ++= codeDef.instances
       }
     }
   }

   private val stemmer = new Stemmer

   private def stem(term: String) = {
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

class CodeTable(val name: Option[String] = None) {
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
      toText.foreach{ p.println }
    }
    finally { 
      p.close() 
    }
  }
}

object CodeTable {

  def parseText(lines: Iterator[String], name: Option[String] = None): CodeTable = {
     val startDef = new Regex("""^(\w+)\s+(.+)$""")
     val continueDef = new Regex("""^-\s+([^\#]+)(.*)$""")
  
     var result = new CodeTable(name)
     var lineno = 1
     var currentDef: Option[CodeDef] = None
  
     lines.foreach { z =>
       try{
         z match {
           case startDef(code, desc) => { 
             if(currentDef != None)
               result.add(currentDef.get)
             currentDef = Some(CodeDef(id = code, desc = desc))
           }
           case continueDef(synonym, annotation) => {
             if(currentDef == None)
               throw new RuntimeException("line %d: '%s' has wrong format".format(lineno,z))
             else
               currentDef.get.instances += CodeInst(synonym,Origin.parseText(annotation))
           }  
           case _ => throw new RuntimeException("Error line '%s' has wrong format".format(z))
         }  
       } catch {
         case ex: Exception => throw new RuntimeException("Error when parsing line %d: ".format(lineno), ex)
       }
       lineno += 1 
     }
     if(currentDef != None) result.add(currentDef.get)

     result
   }

   def parseTextFile(fileName: String): CodeTable = {
      parseText(Source.fromFile(fileName).getLines, Some(fileName))     
   }
}
