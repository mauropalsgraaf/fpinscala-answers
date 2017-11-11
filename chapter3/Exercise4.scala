package chapter3

object Exercise4 {
    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
        case Nil => List()
        case x :: xs if f(x) => dropWhile(xs)(f)
        case _ => l
    }

    def main(args: Array[String]): Unit = {
        println(dropWhile(List(2, 4, 6, 7, 8): List[Int])(x => x % 2 == 0))
    }
}
