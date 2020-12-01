package StrictnessAndLaziness

object SAL {

  sealed trait Stream[+A] {

    import Stream._

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(hd, _) => Some(hd())
    }

    def toList: List[A] = this match {
      case Cons(h, t) => h() :: t().toList
      case Empty => Nil
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, _) if n == 1 => cons(h(), Empty)
      case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
      case _ => Empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n == 0 => t()
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => Empty
    }

    def takeWhile(f: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
      case Cons(h, t) if (f(h())) => t().takeWhile(f)
      case _ => Empty
    }
  }

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  case object Empty extends Stream[Nothing]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))


  }
  def main(args: Array[String]): Unit = {

    val stream = Stream(1, 2, 3, 4, 5, 6, 7)
    println(stream.takeWhile(_ < 10).toList(10))
    println(stream.take(3).toList(10))
    println(stream.drop(2).toList(10))
  }

}



