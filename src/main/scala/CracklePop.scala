/*
  Write a program that prints out the numbers 1 to 100 (inclusive).
  If the number is divisible by 3, print Crackle instead of the number.
  If it's divisible by 5, print Pop.
  If it's divisible by both 3 and 5, print CracklePop.

  Tested on Scala 2.13
*/
object CracklePop {

  def main(args: Array[String]): Unit = {
    (1 to 100).foreach(cracklePop)
  }

  private def cracklePop(i: Int): Unit = {
    i match {
      case _ if i % 3 == 0 && i % 5 == 0 =>
        println("CracklePop")

      case _ if i % 3 == 0 =>
        println("Crackle")

      case _ if i % 5 == 0 =>
        println("Pop")

      case _ =>
        println(i)
    }
  }

}
