package chapter10

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Test {
  implicit val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2

    def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2

    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    def zero: Option[A] = None
  }

  val sortedMonoid: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = if (a1 > a2) a1 else a2

    def zero: Int = Integer.MIN_VALUE
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 andThen a2

    def zero: A => A = x => x
  }

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(a), Stub(b)) => Stub(a + b)
      case (Stub(a), Part(l, count, r)) => Part(a + l, count, r)
      case (Part(l, count, r), Stub(a)) => Part(l, count, r + a)
      case (Part(l1, count1, r1), Part(l2, count2, r2)) =>
        Part(l1, count1 + (if ((r1 + l2).isEmpty) 0 else 1) + count2, r2)
    }

    def zero: WC = Stub("")
  }

  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(a1: (A, B), a2: (A, B)): (A, B) = (a.op(a1._1, a2._1), b.op(a1._2, a2._2))
    def zero: (A, B) = (a.zero, b.zero)
  }

  def functionMonoid[A, B](m: Monoid[B]): Monoid[A => B] = new Monoid[(A) => B] {
    def op(a1: (A) => B, a2: (A) => B): (A) => B = a => { m.op(a1(a), a2(a)) }
    def zero: (A) => B = _ => m.zero
  }

//  def bag[A](as: IndexedSeq[A]): Map[A, Int] =

  def count(s: String): Int = {
    def wc(c: Char): WC =
    if (c.isWhitespace)
      Part("", 0, "")
    else
      Stub(c.toString)

    def unstub(s: String) = s.length min 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(a1: A, a2: A): A = m.op(a2, a1)

    def zero: A = m.zero
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.zero)(m.op)

  def foldMapAlt[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty) m.zero
    else if (v.length == 1) f(v(0))
    else {
      val (l, r) = v.splitAt(v.length / 2)

      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  def sum(xs: List[Int])(implicit monoid: Monoid[Int]): Int = {
    xs.reduce(monoid.op)
  }

  def isSorted(xs: IndexedSeq[Int]): Boolean = {
    val result = foldMapV(xs, sortedMonoid)(x => x)
    
    if (result == xs.last) true
    else false
  }

  def main(args: Array[String]): Unit = {
    println(isSorted(IndexedSeq(0, 0, 0, 1)))
  }
}
