package chapter10

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())((a, b) => a :: b)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Foldable {
  def foldableList: Foldable[List] = new Foldable[List] {
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B = as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
  }

  def foldableIndexedSeq: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {
    def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(mb: Monoid[B]): B = as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
  }

  def foldableTree: Foldable[Tree] = new Foldable[Tree] {
    def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
      case Leaf(x) => f(z, x)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }
    def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
      case Leaf(x) => f(x, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
    def foldMap[A, B](as: Tree[A])(f: (A) => B)(mb: Monoid[B]): B = as match {
      case Leaf(x) => f(x)
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }
  }

  def foldableOption: Foldable[Option] = new Foldable[Option] {
    def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
      case None => z
      case Some(x) => f(z, x)
    }
    def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
      case None => z
      case Some(x) => f(x, z)
    }
    def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B = as match {
      case None => mb.zero
      case Some(x) => f(x)
    }
  }

  def main(args: Array[String]): Unit = {

    println(foldableTree.toList(Branch(Branch(Leaf(3), Leaf(2)), Branch(Leaf(1), Leaf(2)))))
    //println(Foldable.foldableTree.foldRight(Branch(Branch(Leaf(3), Leaf(2)), Branch(Leaf(1), Leaf(2))))(0)((b, a) => b + a))
  }
}
