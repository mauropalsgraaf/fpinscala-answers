package chapter8

import chapter6.{Exercise1, RNG, State}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B, C](b: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2 {
      b.sample
    } {
      f
    })

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(n => f(n).sample))

  def listOfNAlias(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap { n => this.listOfNAlias(n) }

  def unsized: SGen[A] =
    SGen({ _ => this })
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(Exercise1.positiveInt2).map(n => n % 2 == 0))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(Exercise1.positiveInt2).map(n => start + n % (stopExclusive - start)))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap { b => if (b) g1 else g2 }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val number = State(Exercise1.positiveInt2) map { n => n % (g1._2 + g2._2) }

    Gen(number.flatMap { n => if (g1._2 < n) g1._1.sample else g2._1.sample })
  }
}
