package StrictnessAndLaziness

object SAL {

  sealed trait Stream[+A] {

    import Stream._

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(hd, _) => Some(hd())
    }

    def toListRecursive: List[A] = this match {
      case Cons(h, t) => h() :: t().toListRecursive
      case _ => List()
    }

    def toList: List[A] = {
      @annotation.tailrec
      def go(s: Stream[A], acc: List[A]): List[A] = s match {
        case Cons(h, t) => go(t(), h() :: acc)
        case _ => acc
      }

      go(this, List()).reverse
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }


    def toListFast: List[A] = {
      val buf = new collection.mutable.ListBuffer[A]

      @annotation.tailrec
      def go(s: Stream[A]): List[A] = s match {
        case Cons(h, t) =>
          buf += h()
          go(t())
        case _ => buf.toList
      }

      go(this)
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, _) if n == 1 => cons(h(), Empty)
      case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
      case _ => Empty
    }

    def forAll(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) && t().forAll(p)
      case _ => true
    }

    def takeWhileViaFoldRight(f: A => Boolean): Stream[A] =
      foldRight(empty[A])((h, t) =>
        if (f(h)) cons(h, t)
        else empty)

    @annotation.tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

    def takeWhile(f: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
      case _ => Empty
    }

    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
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
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))


  }

  def main(args: Array[String]): Unit = {

    val stream = Stream.apply(1, 2, 3, 4, 5, 6, 7)
    println(stream.takeWhileViaFoldRight(x => x <= 3).toList)

//    println(stream.toList)
//    println(stream.take(2).toList)
//    println(stream.drop(2).toList)

  }

}



