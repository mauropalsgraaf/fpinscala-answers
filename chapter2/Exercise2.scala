package chapter2

object Exercise2 {
    @annotation.tailrec
    def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
        if (as.length <= 1) return true
        if (!gt(as(0), as(1))) return false

        isSorted(as.tail, gt)
    }

    def main(args: Array[String]): Unit = {
        println(isSorted(Array(1, 2, 4, 3), (x: Int, y: Int) => x < y))
    }
}
