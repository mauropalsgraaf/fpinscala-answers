package chapter3

object Exercise23 {
    def zip[A](l1: List[A], l2: List[A])(f: (A, A) => A) : List[A] = (l1, l2) match {
        case (Nil, Nil) => List()
        case (Nil, _) => sys.error("Lists not of same length")
        case (_, Nil) => sys.error("Lists not of same length")
        case (x1 :: xs1, x2 :: xs2) => f(x1, x2) :: zip(xs1, xs2)(f)
    }

    def main(args: Array[String]): Unit = {
        println(zip(List(1, 2, 3), List(4, 5, 6))(_ + _))
    }
}
