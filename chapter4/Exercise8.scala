package chapter4

object Exercise8 {
    def sequence[A, E](l: List[Either[E, A]]): Either[E, List[A]] = l match {
        case Nil => Right(Nil)
        case x :: xs => x flatMap (a => sequence(xs) map (b => a :: b))
    }

    def traverse[A, B, E](l: List[A])(f: A => Either[E, B]): Either[E, List[B]] = l match {
        case Nil => Right(Nil)
        case x :: xs => f(x) flatMap (a => traverse(xs)(f) map (b => a :: b))
    }
}
