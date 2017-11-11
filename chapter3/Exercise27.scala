package chapter3

object Exercise27 {
    def depth[A](tree: Tree[A]): Int = tree match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

    def main(args: Array[String]): Unit = {
        println(depth(Branch(Branch(Branch(Leaf(1), Branch(Leaf(1), Leaf(1))), Leaf(2)), Leaf(5))))
    }
}
