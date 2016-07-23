import scala.annotation.tailrec

object Main extends App{
  println("hello world")
  val list = Range(0,10).toList
  println("last item is:%s".format(last(list)))
  //Write a function last : 'a list -> 'a option that returns the last element of a list. (easy)

  @tailrec
  def last[A](list:List[A]): Option[A] =  list match{
    case Nil => None
    case h::Nil => Some(h)
    case h::t => last (t)
  }

}