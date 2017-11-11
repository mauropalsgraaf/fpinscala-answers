package chapter3

object Exercise12 {
    def reverse[A](l: List[A]): List[A] = l.foldLeft(List(): List[A])((b, a) => a :: b)

    def main(args: Array[String]): Unit = {
        println(reverse(List(1, 2, 3)))
    }
}
