package chapter12

import chapter11.Monad

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = {
    as.foldRight(unit(List.empty: List[B]))((a: A, acc: F[List[B]]) => map2(f(a), acc)(_ :: _))
  }

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldLeft(unit(List[A]()))((b, a) => map2(a, b)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    if (n <= 0) unit(List[A]())
    else map2(fa, replicateM(n - 1, fa))(_ :: _)

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map()): F[Map[K, V]])((mapItem, acc) => {
      val (k, v) = mapItem
      map2(v, acc)((x, y) => y + (k -> x))
    })
}

sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

object Validation {
  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    def unit[A](a: => A): Validation[E, A] = Success(a)
    def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = fa match {
      case Success(a) => fb match {
        case Success(b) => unit(f(a, b))
        case e@Failure(_, _) => e
      }
      case e@Failure(x, xs) => fb match {
        case Success(_) => e
        case Failure(y, ys) => Failure(x, xs ++ Vector(y) ++ ys)
      }
    }
  }
}

object Either {
  def Either[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    def unit[A](a: => A): Either[E, A] = Right(a)

    def flatMap[A, B](ma: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] = ma match {
      case Left(e) => Left(e)
      case Right(x) => f(x)
    }
  }
}

trait Applicative2[F[_]] extends Functor[F] {
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f): F[A => B])(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried): F[B => C])(fb)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(map(fa)(f.curried): F[B => C => D])(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(map(fa)(f.curried): F[B => C => D => E])(fb))(fc))(fd)
}
