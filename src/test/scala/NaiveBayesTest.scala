import org.scalatest.FunSuite
 
class NaiveBayesTest extends FunSuite {
 
  test("NB sample") {
     case class Country(val name: String)
     val china = Country("china")
     val other = Country("other")

     val nb = new NaiveBayes[Country]

     nb.train(china,"Chinese Beijing Chinese".split(" ").elements)
     nb.train(china,"Chinese Chinese Shanghai".split(" ").elements)
     nb.train(china,"Chinese Macao".split(" ").elements) 
     nb.train(other,"Tokyo Japan Chinese".split(" ").elements) 
     val (country,score) = nb.apply("Chinese Chinese Chinese Tokyo Japan".split(" ").elements)
     println(score)
     expect(country) { china }
  }
 
}
