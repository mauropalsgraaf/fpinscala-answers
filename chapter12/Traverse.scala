package chapter12

import chapter10.Foldable

trait Traverse[F[_]] extends Functor[F] {
//  def traverse[M[_] : Applicative, A, B](fa: F[A])(f: A => M[B]): M[F[B]] =
//    sequence(map(fa)(f))

//  def sequence[M[_] : Applicative, A](fma: F[M[A]]): M[F[A]] =
//    traverse(fma)(ma => ma)
}

object Traverse {
//  def listTraverse = new Traverse[List] {
//
//  }
}