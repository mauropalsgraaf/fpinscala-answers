package chapter6

import chapter6.RNG.Rand

object Exercise5 {
  def double: Rand[Double] = {
    RNG.map(Exercise1.positiveInt2)(x => x.toDouble / (Integer.MAX_VALUE.toDouble + 1))
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldLeft(RNG.unit(List[A]()))((b, a) => RNG.map2(b, a)((x, y) => y :: x))
  }
}
