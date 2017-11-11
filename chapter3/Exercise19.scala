package chapter3

object Exercise19 {
    def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
        case Nil => List()
        case x :: xs if f(x) => x :: filter(xs)(f)
        case x :: xs if !f(x) => filter(xs)(f)
    }

    def main(args: Array[String]): Unit = {
        println(filter(List(1, 2, 3))(x => x % 2 == 0))
    }
}
