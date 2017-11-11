package chapter4

object Exercise3 {
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
        a flatMap (x => b map (y => f(x, y)))
    }

    def map2_1[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
        for {
            x <- a
            y <- b
        } yield f(x, y)
    }

    def main(args: Array[String]): Unit = {
        println(map2_1(Some(3), Some(5))(_ + _))
    }
}
