package chapter4

object Exercise2 {
    def variance(xs: Seq[Double]): Option[Double] = {
        mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    }

    def mean(l: Seq[Double]): Option[Double] = {
        if (l.isEmpty) None
        else Some(l.sum / l.length)
    }
}
