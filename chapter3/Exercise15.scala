package chapter3

object Exercise15 {
    def concat[A](l1: List[A], l2: List[A]): List[A] = l2.foldLeft(l1)((b, a) => b :+ a)

    def main(args: Array[String]): Unit = {
        println(concat(List(1, 2, 3), List(4, 5, 6))) //List(1, 2, 3, 4, 5, 6)
    }
}
