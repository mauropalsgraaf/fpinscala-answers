package chapter3

object Exercise16 {
    def add1(l: List[Int]): List[Int] = l match {
        case Nil => List()
        case x :: xs => x + 1 :: add1(xs)
    }

    def main(args: Array[String]): Unit = {
        println(add1(List(1, 2, 3)))
    }
}
