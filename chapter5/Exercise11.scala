package chapter5

object Exercise11 {
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
        case Some((h, t)) => Stream.cons(h, unfold(t)(f))
        case None => Empty
    }
}
