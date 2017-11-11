package chapter3

object Exercise3 {
    def drop[A](l: List[A], n: Int): List[A] = (n, l) match {
        case (_, Nil) => List()
        case (a, xs) if a <= 0 => xs
        case (_, _ :: xs) => drop(xs, n - 1)
    }

    def main(args: Array[String]): Unit = {
        println(drop(List(1, 2), 1))
    }
}
