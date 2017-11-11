package chapter3

object Exercise29 {
    def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
        case Leaf(x) => f(x)
        case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def size[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(1 + _ + _)

    def maximum(tree: Tree[Int]): Int = fold(tree)(a => a)(_ max _)

    def depth[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((d1, d2) => 1 + (d1 max d2))

    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
