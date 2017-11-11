package chapter5

object Exercise12 {
    def fibs(): Stream[Int] = {
        Exercise11.unfold((1, 0))(x => Some((x._1 + x._2, (x._1 + x._2, x._1))))
    }

    def from(n: Int): Stream[Int] = {
        Exercise11.unfold(n)(x => Some((x, x + 1)))
    }

    def constant[A](con: A): Stream[A] = {
        Exercise11.unfold(con)(x => Some((x, x)))
    }

    def onces(): Stream[Int] = {
        constant(1)
    }

    def main(args: Array[String]): Unit = {
        println(onces().take(200).toList)
    }
}
