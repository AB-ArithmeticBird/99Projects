package First

import scala.annotation.tailrec

object Main extends App {
  println("hello world")
  val list = Range(0, 10).toList
  println("last item is:%s".format(last(list)))
  println("Last two element is: %s".format(last_two(list)))
  println("kth element is:" + at(list, 4))
  println("Length: %d".format(length(list)))

  //Write a function last : 'a list -> 'a option that returns the last element of a list. (easy)
  @tailrec
  def last[A](list: List[A]): Option[A] = list match {
    case Nil => None
    case h :: Nil => Some(h)
    case h :: t => last(t)
  }

  //Find the last but one (last and penultimate) elements of a list. (easy)
  @tailrec
  def last_two[A](list: List[A]): (Option[A], Option[A]) = list match {
    case Nil => (None, None)
    case h :: t => t match {
      case Nil => (Some(h), None)
      case th :: Nil => (Some(h), Some(th))
      case th :: tt => last_two(t)
    }
  }

  //Find the k'th element of a list. (easy)
  @tailrec
  def at[A](list: List[A], n: Int): Option[A] = list match {
    case Nil => None
    case h :: t => if (n == 0) Some(h) else at(t, n - 1)
  }

  //Find the number of elements of a list. (easy)
  def length[A] (list: List[A]): Int = {
    @tailrec
    def aux(l: List[A], len: Int): Int = l match {
      case Nil => len
      case h :: t => aux(t, len + 1)
    }
    aux(list, 0)
  }

}