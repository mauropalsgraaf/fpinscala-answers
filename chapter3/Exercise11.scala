package chapter3

object Exercise11 {
    def sum(l: List[Int]): Int = l.foldLeft(0)(_ + _)
    def product(l: List[Int]): Int = l.foldLeft(1)(_ * _)
    def length(l: List[Int]): Int = l.foldLeft(0)((b, _) => b + 1)
}
