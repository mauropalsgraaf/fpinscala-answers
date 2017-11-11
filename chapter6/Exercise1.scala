package chapter6

object Exercise1 {
  def positiveInt(rng: RNG): Option[(Int, RNG)] = rng.nextInt match {
    case (Integer.MIN_VALUE, _) => None
    case (x, nextRng) => Some((Math.abs(x), nextRng))
  }

  def positiveInt2(rng: RNG): (Int, RNG) = {
    val (x, nextRng) = rng.nextInt

    if (x < 0)
      (-(x + 1), nextRng)
    else
      (x, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (x, nextRng) = positiveInt2(rng)

    (x.toDouble / (Integer.MAX_VALUE.toDouble + 1), nextRng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (x, rng2) = positiveInt2(rng)
    val (y, rng3) = double(rng2)

    ((x, y), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((x, y), nextRng) = intDouble(rng)

    ((y, x), nextRng)
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (x, rng2) = double(rng)
    val (y, rng3) = double(rng2)
    val (z, rng4) = double(rng3)

    ((x, y, z), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) return (List(), rng)

    val (x, nextRng) = rng.nextInt
    (x :: ints(count - 1)(nextRng)._1, rng)
  }

  def main(args: Array[String]): Unit = {
    println(ints(5)(RNG.simple(230192309)))
  }
}
