package chapter8

case class SGen[A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] =
    SGen(n => forSize(n) map f)

  def map2[B, C](sgen2: SGen[B])(f: (A, B) => C): SGen[C] =
    SGen { n => forSize(n).map2(sgen2.forSize(n))(f) }

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen { n => forSize(n).flatMap(x => f(x).forSize(n)) }
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen { n => g.listOfNAlias(n) }
}
