package chapter5

object Exercise8 {
    def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

    def main(args: Array[String]): Unit = {
        println(constant("Hello").take(5).toList)
    }
}
