package chapter4

object Exercise5 {
    def sequence[A](xs: List[Option[A]]): Option[List[A]] = xs match {
        case Nil => Some(List())
        case h :: t => h flatMap (hh => sequence(t) map (xss => hh :: xss))
    }
}
