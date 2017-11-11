package chapter3

object Exercise18 {
    def map[A, B](l: List[A])(f: A => B): List[B] = l match {
        case Nil => List.empty
        case x :: xs => f(x) :: map(xs)(f)
    }

    def main(args: Array[String]): Unit = {
        println(map(List(1, 2, 3))(_ * 3))
    }
}
