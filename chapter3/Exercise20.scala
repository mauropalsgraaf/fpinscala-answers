package chapter3

object Exercise20 {
    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match {
        case Nil => List()
        case x :: xs => Exercise18.map(l)(f).foldLeft(List.empty: List[B])((b ,a) => List.concat(b, a))
    }

    def main(args: Array[String]): Unit = {
        println(flatMap(List(1, 2, 3))(x => List(x, x, x)))
    }
}
