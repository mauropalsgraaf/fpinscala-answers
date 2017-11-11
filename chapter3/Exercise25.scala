package chapter3

object Exercise25 {
    def sum[A](tree: Tree[A]): Int = tree match {
        case Leaf(x) => 1
        case Branch(l, r) => 1 + sum(l) + sum(r)
    }

    def main(args: Array[String]): Unit = {
        println(sum(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1))))
    }
}
