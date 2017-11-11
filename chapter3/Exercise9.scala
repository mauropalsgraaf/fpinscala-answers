package chapter3

object Exercise9 {
    def length[A](l: List[A]): Int = l.foldRight(0)((_, x) => x + 1)

    def main(args: Array[String]): Unit = {
        println(length(List()))
    }
}
