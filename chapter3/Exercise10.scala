package chapter3

object Exercise10 {
    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
        case Nil => z
        case x :: xs => foldLeft(xs, f(z, x))(f)
    }

    def main(args: Array[String]): Unit = {
        println(foldLeft(List(1, 2, 3), 0)(_ + _))
    }
}
