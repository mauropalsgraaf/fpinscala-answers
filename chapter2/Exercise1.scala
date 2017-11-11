package chapter2

object Exercise1 {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int, prev: Int): Int = {
      if (n == 0) return acc

      go(n - 1, acc + prev, acc)
    }

    go(n, 0, 1)
  }

  def main(args: Array[String]): Unit = fib(5)
}
