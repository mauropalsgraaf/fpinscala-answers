package chapter3

object Exercise28 {
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
        case Leaf(x) => Leaf(f(x))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    def main(args: Array[String]): Unit = {
        println(map(Branch(Branch(Branch(Leaf(1), Branch(Leaf(1), Leaf(1))), Leaf(2)), Leaf(5)))(x => x * 3))
    }
}
