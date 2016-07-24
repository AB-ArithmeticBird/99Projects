package onetoten

import scala.annotation.tailrec

object Problem extends App {

  sealed trait Node[A]
  case class One[A](value:A) extends Node[A]
  case class Many[A](value: List[Node[A]]) extends Node[A]

  println("hello world")
  val list = Range(0, 10).toList
  println("List is %s".format(list))
  println("last item is:%s".format(last(list)))
  println("Last two element is: %s".format(last_two(list)))
  println("kth element is:" + at(list, 4))
  println("Length: %d".format(length(list)))
  println("reverse is "+ reverse(list))
  println("is palindrome? %s".format(isPalindrome(list)))

  val example: List[Node[String]] = List(One("a"), Many(List(One("b"), Many(List(One("c"), One("d"))), One("e"))))
  println("flattening "+ flatten(example))

  val duplicateList = List("a","a","a","a","b","c","c","a","a","d","e","e","e","e")
  println("remove subsequent duplicates: %s".format(compress(duplicateList)))

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

  //Reverse a list. (easy)

  def reverse[A](list:List[A]) :List[A] = {
    def aux(l:List[A], accumulator: List[A]):List[A] = l match {
      case Nil => accumulator
      case h::t => aux(t, h::accumulator)
    }
    aux(list, List())
  }

  def isPalindrome[A](list: List[A]): Boolean = list == reverse(list)

  //Flatten a nested list structure. (medium)
  def flatten[A](list: List[Node[A]]):List[A] = {

    @tailrec
    def aux(l:List[Node[A]], acc:List[A]): List[A]= l match {
      case Nil => acc
      case h :: t => h match {
        case One(v) => aux(t, v::acc)
        case Many(li) => aux(li, acc)
      }
    }

   reverse(aux(list, List[A]()))
  }

  @tailrec
  def dropWhile[A](list: List[A], elem: A): List[A] = list match {
      case Nil => Nil
      case h :: t => if (!(h == elem)) list else dropWhile(t, elem)
    }
  //Eliminate consecutive duplicates of list elements. (medium)
  def compress[A](list: List[A]): List[A] = {
    @tailrec
    def doCompress(lst: List[A], result: List[A]): List[A] = lst match {
      case Nil => result
      case h :: t => doCompress(dropWhile(t, h), h :: result)
    }
    reverse(doCompress(list, Nil))
  }

}



