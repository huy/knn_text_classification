import org.scalatest.FunSuite
 
class NaiveBayesTest extends FunSuite {
 
  test("NB sample") {
     case class Country(val name: String)
     val china = Country("china")
     val other = Country("other")

     val nb = new NaiveBayes[Country]

     Map("Chinese Beijing Chinese"->china,
         "Chinese Chinese Shanghai"->china,
         "Chinese Macao"->china,
         "Tokyo Japan Chinese"->other).foreach{ case (str,country)=>
       nb.train(country,str.split(" "))
     }
     println(nb.info)

     expect(china) { nb.apply("Chinese Chinese Chinese Tokyo Japan".split(" "))._1 }
     expect(other) { nb.apply("Tokyo Japan".split(" "))._1 }
  }
 
}
