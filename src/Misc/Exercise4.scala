package Misc

object Exercise4 {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def sequence[A](l: List[Option[A]]): Option[List[A]] = l match {
    case Nil => Some(Nil)
    case h :: t => h match {
      case None => None
      case Some(head) => sequence(t) match {
        case None => None
        case Some(left) => Some(head :: left)
      }
    }
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => f(h) match {
      case None => None
      case Some(validHead) => traverse(t)(f) match {
        case None => None
        case Some(validTail) => Some(validHead :: validTail)
      }
    }
  }

  def seqByTraverse[A](l: List[Option[A]]): Option[List[A]] = traverse(l)(x => x)


  def main(args: Array[String]): Unit = {

    val list1 = List(1, 2, 3, 4)

    println(traverse(list1)(a => if (a == 10) None else Some(a)))
    val list2 = List(Some(1), Some(2), Some(3), None)
    println(seqByTraverse(list2))
    println(seqByTraverse(list2.slice(0, 3)))
  }
}
