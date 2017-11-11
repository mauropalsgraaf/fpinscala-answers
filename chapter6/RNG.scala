package chapter6

trait RNG {
    def nextInt: (Int, RNG)
}

object RNG {
    type Rand[+A] = RNG => (A, RNG)

    def simple(seed: Long): RNG = new RNG {
        def nextInt: (Int, RNG) = {
            val seed2 = (seed * 0x5DEECE66DL + 0xBL) &
                ((1L << 48) - 1)
            ((seed2 >>> 16).asInstanceOf[Int],
                simple(seed2))
        }
    }

    def unit[A](a: A): Rand[A] = {
        rng => (a, rng)
    }

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
        rng => {
            val (x, nextRng) = s(rng)
            (f(x), nextRng)
        }
    }

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
        rng => {
            val (a, rng2) = ra(rng)
            val (b, rng3) = rb(rng2)

            (f(a, b), rng3)
        }
    }

    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
        rng => {
            val (x, nextRng) = f(rng)

            g(x)(nextRng)
        }
    }

    def map1Alt[A, B](s: Rand[A])(f: A => B): Rand[B] = {
        flatMap(s) { a => unit(f(a)) }
    }

    def map2Alt[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
        flatMap(ra)(a => map(rb)(b => f(a, b)))
    }
}