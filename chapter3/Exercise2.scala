package chapter3

object Exercise2 {
    def tail[A](l: List[A]): List[A] = l match {
        case Nil => List()
        case _ :: xs => xs
    }

    def main(args: Array[String]): Unit = println(tail(List(1, 2, 3)))
}
