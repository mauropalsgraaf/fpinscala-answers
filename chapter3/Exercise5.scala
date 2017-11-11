package chapter3

object Exercise5 {
    def setHead[A](l: List[A])(x: A): List[A] = l match {
        case Nil => List()
        case _ :: xs => x :: xs
    }

    def main(args: Array[String]): Unit = {
        println(setHead(List(1, 2, 3))(4)) // List(4, 2, 3)
    }
}
