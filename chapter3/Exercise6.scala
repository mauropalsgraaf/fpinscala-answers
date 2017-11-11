package chapter3

object Exercise6 {
    def init[A](l: List[A]): List[A] = {
        @annotation.tailrec
        def go(l: List[A], acc: List[A]): List[A] = l match {
            case Nil => acc
            case _ :: Nil => acc
            case x :: xs => go(xs, acc :+ x)
        }

        go(l, List())
    }

    def main(args: Array[String]): Unit = {
        println(init(List(1, 2, 3))) // List(1, 2)
    }
}
