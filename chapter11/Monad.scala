package chapter11

import chapter6.State

import scala.language.reflectiveCalls

trait Monad[F[_]] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldLeft(unit(List[A]()))((b, a) => map2(a, b)((x, y) => x :: y))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldLeft(unit(List[B]()))((b, a) => map2(f(a), b)((x, y) => x :: y))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    if (n <= 0) unit(List())
    else map2(ma, replicateM(n - 1, ma))(_ :: _)

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
    map2(ma, mb)((_, _))

  def filterM[A](ma: List[A])(f: A => F[Boolean]): F[List[A]] =
    ma.foldLeft(unit(List[A]()))((b, a) => map2(f(a), b)((x, y) => if (x) a :: y else y))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def _flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_ => ma): Unit => F[A], f)(())

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(identity)

  def __flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  def _compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(g))
}

object Monad {
  def optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)

    def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma match {
      case Some(x) => f(x)
      case None => None
    }
  }

  def listMonad = new Monad[List] {
    def unit[A](a: => A) = List(a)

    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
  }

  def idMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)

    def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = ma flatMap f
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] =
      ma flatMap f
  }

  def main(args: Array[String]): Unit = {
    val result = stateMonad[Int].map2(State(x => {val r = (x, x * 2); println(r); r}), State(x => {val r = (x, x * 2); println(r); r}))((a, b) => a + b).run(10)

    println(result)
  }
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] =
    Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] =
    f(value)
}

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    def flatMap[A, B](ma: Reader[R, A])(f: (A) => Reader[R, B]): Reader[R, B] =
      Reader(
        r => f(ma.run(r)).run(r)
      )
  }

}