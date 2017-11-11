package chapter3

object Exercise14 {
    def append[A](l: List[A])(x: A): List[A] = l.foldLeft(List(x))((b, a) => b :+ a)

    def main(args: Array[String]): Unit = println(append(List(1, 2, 3))(0))
}
