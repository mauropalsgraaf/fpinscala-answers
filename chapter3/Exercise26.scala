package chapter3

object Exercise26 {
    def maximum(tree: Tree[Int]): Int = tree match {
        case Leaf(x) => x
        case Branch(l, r) => maximum(l) max maximum(r)
    }

    def main(args: Array[String]): Unit = {
        println(maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(5))))
    }
}
