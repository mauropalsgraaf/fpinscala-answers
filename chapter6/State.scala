package chapter6

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = {
    State(
      state => {
        val (x, nextState) = run(state)

        (f(x), nextState)
      }
    )
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    State(
      state => {
        val (a, state2) = run(state)
        val (b, state3) = sb.run(state2)
        (f(a, b), state3)
      }
    )
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(
      state => {
        val (x, state3) = run(state)

        f(x).run(state3)
      }
    )
  }
}

object State {
  def unit[S, A](x: A): State[S, A] = {
    State(state => {
      (x, state)
    })
  }

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    fs.foldLeft(unit(List()): State[S, List[A]]) {
      (acc, a) => acc.map2(a)((x, y) => y :: x)
    }
  }
}
