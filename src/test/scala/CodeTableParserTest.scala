import org.scalatest.FunSuite
 
class CodeTableParserTest extends FunSuite {
 
  test("ascii parser") {
     val codeTable = CodeTable.parseText(
       """177     Administrator
-       office administrator
-       sales administrator
-       administration manager""".linesIterator
     )
     expect(1) { codeTable.size }
     expect(CodeDef(id="177",desc="Administrator")) { codeTable(0) }
     expect(3) { codeTable(0).instances.size }
     expect(CodeInstance(desc="administration manager")) { codeTable(0).instances.last }
  }
 
}
