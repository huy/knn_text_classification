import org.scalatest.FunSuite
 
class CodeTableParserTest extends FunSuite {
 
  test("parse ascii text") {
     val codeTable = CodeTable.parseText(
       """177     Administrator
-       office administrator
-       sales administrator
-       administration manager""".linesIterator
     )
     expect(1) { codeTable.size }
     expect(CodeDef(id="177",desc="Administrator")) { codeTable(0) }
     expect(3) { codeTable(0).instances.size }
     expect(CodeInst(desc="administration manager")) { codeTable(0).instances.last }
  }

  test("termSeq of CodeDef") {
     var codeDef = CodeDef(id="428",desc="BD Executive/Manager")
     codeDef.instances += CodeInst("business development manager")
     codeDef.instances += CodeInst("manager of planning and reporting")

     expect(List("BD","Executive","Manager",
                 "business","development","manager",
                 "manager","of","planning","and","reporting")) {
       codeDef.termSeq
     }
  }
 
}
