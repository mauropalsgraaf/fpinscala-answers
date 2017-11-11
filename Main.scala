object Main {
  def sum2(list: List[Int]): Int = list match {
    case Nil => 0
    case x :: xs => x + sum(xs)
  }

  def sum(list: List[Int]): Int = {
    def loop(list: List[Int], acc: Int): Int = list match {
      case Nil => acc
      case x :: xs => loop(xs, acc + x)
    }

    loop(list, 0)
  }

  def main(args: Array[String]): Unit = {
    println(sum(List(1, 2, 3)))
  }

  sealed trait Tree[A]
  case class Node[A](l: Tree[A], value: A, r: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]

  object Tree {
    val tree: Tree[Int] = Node(Node(Node(Leaf(1), 3, Node(Leaf(4), 5, Leaf(2))), 1, Leaf(3)), 4, Leaf(8))

    def sum(tree: Tree[Int]): Int = tree match {
      case Leaf(x) => x
      case Node(l, x, r) => sum(l) + x + sum(r)
    }
  }
}
