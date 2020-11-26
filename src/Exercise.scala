import scala.annotation.tailrec

object Exercise {
  //exercise 2.1
  def fibonacci(n: Int): Int = {
    if (n == 0) 0
    else if (n <= 2) {
      1
    } else fibonacci(n - 1) + fibonacci(n - 2)
  }

  //exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else if (!ordered(as(n - 1), as(n))) false
      else loop(n + 1)
    }

    loop(1)
  }

  //exercise 2.3
  /*
  Let’s look at another example, currying,9 which converts a function f of two arguments
into a function of one argument that partially applies f. Here again there’s only one
implementation that compiles. Write this implementation.
  * */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  //exercise 2.4
  /*
  * Implement uncurry, which reverses the transformation of curry.
  * Note that since => associates to the right, A => (B => C) can be written as A => B => C.
  * */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  //exercise 2.5
  /*
  * Implement the higher-order function that composes two functions.
  * */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  //exercise 3.2
  /*
  * Implement the function tail for removing the first element of a List.
  * Note that the function takes constant time.
  * What are different choices you could make in your implementation if the List is Nil?
  * */
  def tail(as: List[String]): List[String] = {
    as match {
      case Nil => Nil
      case _ :: xs => xs
    }
  }

  def main(args: Array[String]): Unit = {
    //    println(isSorted(Array(1, 2, 2, 4, 5), (i: Int, j: Int) => {
    //      i <= j
    //    }))
    println(tail(List("123", "2345", "hello")))
    ()
  }
}
