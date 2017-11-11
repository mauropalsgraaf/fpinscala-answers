package chapter5

trait Stream[+A] {
    def toList: List[A] = this match {
        case Empty => List.empty
        case Cons(h, t) => h() :: t().toList
    }

    def take(n: Int): Stream[A] = this match {
        case Empty => Stream.empty
        case Cons(_, _) if n <= 0 => Stream.empty
        case Cons(h, t) => Stream.cons(h(), t().take(n - 1))
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case Empty => z
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
        case Cons(h, t) if p(h()) => Stream.cons(h(), t() takeWhile p)
        case _ => Stream.empty
    }

    def drop(n: Int): Stream[A] = {
        if (n <= 0) this
        else this match {
            case Empty => Stream()
            case Cons(h, t) => t().drop(n - 1)
        }
    }

    def forAll(p: A => Boolean): Boolean = this match {
        case Empty => true
        case Cons(h, t) if p(h()) => t().forAll(p)
        case _ => false
    }

    def forAllAlt(p: A => Boolean): Boolean =
        foldRight(true)((a, b) => p(a) || b)

    def takeWhileAlt(p: A => Boolean): Stream[A] =
        foldRight(Stream.empty: Stream[A])((h, acc) => if (p(h)) Stream.cons(h, acc) else Stream.empty)

    def headOption(): Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

    def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((a, b) => Stream.cons(f(a), b))

    def filter(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if (p(a)) return b else Stream.cons(a, b))

    def append[B >: A](s: Stream[B]): Stream[B] = foldRight(s)((a, b) => Stream.cons(a, b))

    def flatMap[B](f: A => Stream[B]): Stream[B] = (this map f).foldRight { Empty: Stream[B] } { (a, b) => a.append(b) }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
    def empty[A]: Stream[A] = Empty

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = Cons(() => hd, () => tl)

    def apply[A](as: A*): Stream[A] =
        if (as.isEmpty) Empty
        else cons(as.head, apply(as.tail: _*))

    def main(args: Array[String]): Unit =
        println(Stream(2, 2, 4, 6, 7, 8).flatMap(a => Stream.cons(a, Stream.cons(a, Empty))).toList)

}