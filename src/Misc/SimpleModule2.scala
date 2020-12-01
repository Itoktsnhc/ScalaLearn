package Misc

object SimpleModule2 {

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = {
      this match {
        case Right(a) => Right(f(a))
        case Left(e) => Left(e)
      }
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
      map(f) match {
        case Right(a) => a
        case Left(e) => Left(e)
      }
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(_) => b
      case Left(_) => this
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      this flatMap (aa => b map (bb => f(aa, bb)))
  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

  def main(args: Array[String]): Unit = {
    val right = Right[Int](3)
    val left = Left[String]("Error F")
    println(right.map(_ * 3))
    println(left.map(_ => 3))

    right.flatMap(item => Right(item * 3))
    left.flatMap(_ => Right(3))

  }
}
