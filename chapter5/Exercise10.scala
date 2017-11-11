package chapter5

object Exercise10 {
    def fibs(): Stream[Int] = {
        def go(f0: Int, f1: Int): Stream[Int] =
            Stream.cons(f0, go(f1, f0+f1))
        go(0, 1)
    }

    def main(args: Array[String]): Unit = {
        println(fibs().take(200).toList)
    }
}
