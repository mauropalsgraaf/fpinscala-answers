package chapter13

import chapter11.Monad

sealed trait Free[F[_], A] {
  def map[B](f: A => B): Free[F, B] =
    this flatMap (a => Return(f(a)))

  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    FlatMap(this, f)
}

case class Return[F[_], A](a: A) extends Free[F, A]

case class Suspend[F[_], A](s: F[A]) extends Free[F, A]

case class FlatMap[F[_], A, B](s: Free[F, A],
                               f: A => Free[F, B]) extends Free[F, B]

object Free {
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] = new Monad[({type f[a] = Free[F, a]})#f] {
    def flatMap[A, B](ma: Free[F, A])(f: (A) => Free[F, B]): Free[F, B] = ma flatMap f

    def unit[A](a: => A): Free[F, A] = Return(a)
  }

//  @annotation.tailrec
//  def runTrampoline[A](a: Free[Function0, A]): A = a match {
//    case Return(a) => a
//    case Suspend(r) => r()
//    case FlatMap(x, f) => x match {
//      case Return(a) => runTrampoline {
//        f(a)
//      }
//      case Suspend(r) => runTrampoline {
//        f(r())
//      }
//      case FlatMap(a0, g) => runTrampoline {
//        a0 flatMap { a0 => g(a0) flatMap f }
//      }
//    }
//  }

}