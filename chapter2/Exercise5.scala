package chapter2

object Exercise5 {
    def uncurry[A, B, C](f: A => B => C): (A, B) => C =
        (a, b) => f(a)(b)
}
