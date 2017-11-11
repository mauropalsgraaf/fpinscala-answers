package chapter2

object Exercise4 {
    def curry[A, B, C](f: (A, B) => C): A => (B => C) =
        a => b => f(a, b)
}
