package chapter4

object Exercise6 {
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
        case Nil => Some(List())
        case x :: xs => f(x) flatMap (xx => traverse(xs)(f) map (xx :: _))
    }

    def sequence[A](xs: List[Option[A]]): Option[List[A]] =
        traverse(xs)(a => a)
}
