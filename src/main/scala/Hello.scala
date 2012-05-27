object Hi {
  def main(args: Array[String]) = {
    println("Hi " + args.reduceLeft[String] { (acc,n) => acc + " " + n})
  }
}
