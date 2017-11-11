package chapter3

object Exercise22 {
    def zip(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
        case (Nil, Nil) => List()
        case (Nil, _) => sys.error("Lists not of same length")
        case (_, Nil) => sys.error("Lists not of same length")
        case (x1 :: xs1, x2 :: xs2) => x1 + x2 :: zip(xs1, xs2)
    }

    def main(args: Array[String]): Unit = {
        println(zip(List(1, 2, 3), List(4, 5, 6))) //List(5, 7, 9)
    }
}
