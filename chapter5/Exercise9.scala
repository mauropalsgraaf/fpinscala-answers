package chapter5

object Exercise9 {
    def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

    def main(args: Array[String]): Unit = {
        println(from(1).take(200).toList)
    }
}
