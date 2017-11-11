package chapter2

object Exercise3 {
    def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
        (b: B) => f(a, b)
    }
}
