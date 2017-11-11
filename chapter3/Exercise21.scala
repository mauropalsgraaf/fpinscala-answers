package chapter3

object Exercise21 {
    def filter[A](l: List[A])(f: A => Boolean): List[A] =
        Exercise20.flatMap(l)(a => {
            if (f(a))
                List(a)
            else
                Nil
        })

    def main(args: Array[String]): Unit = {
        println(filter(List(1, 2, 3))(x => x % 2 == 1))
    }
}
