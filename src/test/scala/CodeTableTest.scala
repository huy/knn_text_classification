import org.scalatest.FunSuite
 
class CodeTableTest extends FunSuite {
 
  test("parse ascii text") {
     val codeTable = CodeTable.parseText(
       """177     Administrator
-       office administrator
-       sales administrator
-       administration manager""".linesIterator
     )
     expect(1) { codeTable.size }
     expect(CodeDef(id="177",desc="Administrator")) { 
       codeTable.codeDef("177") }
     expect(3) { 
       codeTable.codeDef("177").instances.size }
     expect(CodeInst(desc="administration manager")) { 
       codeTable.codeDef("177").instances.last }
  }

  test("termSeq of codeDef") {
     var codeDef = CodeDef(id="428",desc="BD Executive/Manager")
     codeDef.instances += CodeInst("business development manager")
     codeDef.instances += CodeInst("manager of planning and reporting")

     expect(List("BD","executive","manager",
                 "business","development","manager",
                 "manager","planning","reporting")) {
       codeDef.termSeq
     }
  }

  test("merge 2 codeDefs") {
     var one = CodeDef(id="242",desc="Manager")
     var other = CodeDef(id="261",desc="Sales Manager")
     other.instances += CodeInst("sales & marketing manager")
     one.merge(other)

     expect(2) {
       one.instances.size
     } 
     expect(List(CodeInst("Sales Manager"),CodeInst("sales & marketing manager"))){
       one.instances
     }
  }

  test("to ascii text"){
    val codeDef = new CodeDef(id="428",desc="BD Executive/Manager")
    codeDef.instances += CodeInst("business development manager")
    codeDef.instances += CodeInst("manager of planning and reporting")

    var codeTable = new CodeTable()
    codeTable.add(codeDef)

    expect("428\tBD Executive/Manager\n-\tbusiness development manager\n-\tmanager of planning and reporting"){
      codeTable.toText.mkString("\n")
    }
  }
 
}
