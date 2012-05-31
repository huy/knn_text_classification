import org.scalatest.FunSuite
 
class NaiveBayesTest extends FunSuite {
 
  test("example 13.1") {
     val nb = new NaiveBayes[String]

     Map("Chinese Beijing Chinese"->"china",
         "Chinese Chinese Shanghai"->"china",
         "Chinese Macao"->"china",
         "Tokyo Japan Chinese"->"other").foreach{ case (str,country)=>
       nb.train(country,str.split(" "))
     }
     println(nb.info)

     expect("china") { nb.apply("Chinese Chinese Chinese Tokyo Japan".split(" "))._1 }
     expect("other") { nb.apply("Tokyo Japan".split(" "))._1 }
  }
 
}
