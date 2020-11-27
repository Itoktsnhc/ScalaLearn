import scala.List
import scala.annotation.tailrec

object Exercise3 {
  //exercise 3.2
  /*
  * Implement the function tail for removing the first element of a List.
  * Note that the function takes constant time.
  * What are different choices you could make in your implementation if the List is Nil?
  * */
  def tail[A](as: List[A]): List[A] = {
    as match {
      case Nil => Nil
      case _ :: xs => xs
    }
  }

  //exercise 3.3
  /*Using the same idea, implement the function setHead for replacing the first element
of a List with a different value.*/
  def setHeader[A](l: List[A], newHead: A): List[A] = {
    l match {
      case Nil => Nil
      case _ :: xs => newHead :: xs
    }
  }

  //EXERCISE 3.4
  /*
  * Generalize tail to the function drop, which removes the first n elements from a list.
Note that this function takes time proportional only to the number of elements being
dropped—we don’t need to make a copy of the entire List.
  *
  * */
  def drop[A](l: List[A], firstN: Int): List[A] = {
    if (l.length <= firstN) List()

    @tailrec
    def loop(n: Int, iAs: List[A]): List[A] = {
      if (n == 0) iAs
      else
        iAs match {
          case _ :: xs => loop(n - 1, xs)
        }
    }

    loop(firstN, l)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    @tailrec
    def loop(iL: List[A]): List[A] = {
      iL match {
        case Nil => Nil
        case x :: xs =>
          if (f(x))
            loop(xs)
          else x :: xs
      }
    }

    loop(l)
  }

  @tailrec
  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case x :: xs if f(x) => dropWhile2(xs)(f)
      case _ => l
    }
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case h :: t => f(h, foldRight(t, z)(f))
    }

  def foldLeft1[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case h :: t => foldLeft1(t, f(z, h))(f)
    }
  }

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case h :: t => foldLeft(t, f(z, h))(f)
  }

  //exercise 3.13
  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(as, z)((a, b) => f(b, a))
  }

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as, z)((a, b) => f(b, a))
  }


  //exercise 3.14
  def appendFold[A](a1: List[A], a2: List[A]): List[A] =
  // foldLeft(reverse(a1), a2)((b: List[A], a: A) => Cons(a, b))
    foldRight(a1, a2)((a: A, b: List[A]) => a :: b)

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case h :: t => h :: append(t, a2)
  }

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)

  def concat1[A](l: List[List[A]]): List[A] = foldLeft(l, Nil: List[A])((x, y) => append(y, x))

  def main(args: Array[String]): Unit = {
    println(setHeader(List(1, 2, 3), 5))
    println(drop(List(1, 2, 3, 4, 5, 6), 3))
    println(dropWhile2(List(1, 2, 3, 4, 5, 6))(a => {
      a <= 3
    }))
    val list = List(1, 2, 3, 4, 5)
    //exercise 3.9 use foldRight to implement length
    println(foldRight(list, 0)((_, y) => y + 1)) //length
    println(foldLeft(list, 0)((x, y) => x + y))
    println(foldLeft(list, List[Int]())((x, y) => y :: x))
    val listOfList = List(list, list, list)
    println(concat(listOfList))
    println(concat1(listOfList))
  }
}