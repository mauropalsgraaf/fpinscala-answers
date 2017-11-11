package chapter3

object Exercise17 {
    def doubleToString(l: List[Double]): List[String] = l match {
        case Nil => List()
        case x :: xs => x.toString :: doubleToString(xs)
    }

    def main(args: Array[String]): Unit = {
        println(doubleToString(List(1.1, 2.2)))
    }
}
