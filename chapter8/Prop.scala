package chapter8

import chapter6.RNG
import chapter8.Prop.{FailedCase, SuccessCount, TestCases}

sealed trait Result {
  def isFalsified: Boolean = this match {
    case Passed => false
    case Falsified(_, _) => true
  }
}

case object Passed extends Result

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(prop: Prop): Prop = Prop {
    (cases, rng) => {
      this.run(cases, rng) match {
        case Passed => prop.run(cases, rng)
        case x => x
      }
    }
  }

  def ||(prop: Prop): Prop = Prop {
    (cases, rng) => {
      this.run(cases,rng) match {
        case Falsified(x, y) => this.run(cases,rng)
        case x => x
      }
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
}