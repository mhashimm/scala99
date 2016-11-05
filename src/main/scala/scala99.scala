import scala.util.Random

object scala99 {
  def last[A](list: List[A]): A = list match {
    case Nil => throw new IllegalArgumentException
    case x :: Nil => x
    case x :: xs => last(xs)
  }

  def penultimate[A](list: List[A]): A = list match {
    //list.reverse.tail.head
    case Nil => throw new IllegalArgumentException
    case x :: y :: Nil => x
    case x :: xs => penultimate(xs)
  }

  def nth[A](index: Int, list: List[A]): A = {
    if (index > 0)
      nth(index - 1, list.tail)
    else
      list.head
  }

  def length[A](list: List[A]): Int = list match {
    //list.foldRight(0)((a,b) => b + 1)
    case Nil => 0
    case x :: xs => 1 + length(xs)
  }

  def reverse[A](list: List[A]): List[A] = list match {
    //list.foldLeft(List.empty[A])((xs, x) => x::xs)
    case Nil => Nil
    case x :: xs => reverse(xs) ::: List(x)
  }

  def isPalindrome[A](list: List[A]): Boolean = {
    val point = if (list.length % 2 == 0) list.length / 2 else list.length / 2 + 1
    val l1 = list.take(list.length / 2)
    val l2 = list.drop(point)
    l1 == reverse(l2)
  }

  def flatten(list: List[Any]): List[Any] = list match {
    case Nil => Nil
    case x :: xs => x match {
      case y :: ys => y :: flatten(ys ::: xs)
      case y => y :: flatten(xs)
    }
  }

  def compress[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case x :: xs if xs.nonEmpty && x == xs.head => compress(xs)
    case x :: xs => x :: compress(xs)
  }

  def pack[A](list: List[A]): List[List[A]] = list.foldRight(List.empty[List[A]])((a, acc) => {
    acc match {
      case x :: xs if x.head == a => (a :: x) :: xs
      case xs => List(a) :: xs
    }
  })

  def encode[A](list: List[A]): List[(Int, A)] = for (p <- pack(list)) yield (p.length, p.head)

  def encodeModified[A](list: List[A]): List[Any] = for (p <- pack(list)) yield {
    if (p.length == 1)
      p.head
    else (p.length, p.head)
  }

  def decode[A](list: List[(Int, A)]): List[A] = for {
    (i, a) <- list
    x <- (0 until i).map(y => a)
  } yield x

  def encodeDirect[A](list: List[A]): List[(Int, A)] = {
    def loop(sa: List[A], i: Int): List[(Int, A)] = sa match {
      case Nil => Nil
      case x :: y :: xs if x == y => loop(y :: xs, i + 1)
      case x :: xs if i == 1 => (i, x) :: loop(xs, 1)
      case x :: xs => (i, x) :: loop(xs, 1)
    }
    loop(list, 1)
  }

  def duplicate[A](list: List[A]): List[A] = list.flatMap(a => List(a, a))

  def duplicateN[A](n: Int, list: List[A]): List[A] = list.flatMap(a => (0 until n).map(b => a))

  /*def drop[A](nth: Int, list: List[A]): List[A] = {
    def loop(n: Int, sa: List[A]): List[A] = sa.zipWithIndex match {
      case Nil => Nil
      case (x, i)::xs if (i + 1) % n == 0 => loop(nth, xs.unzip._1)
      case (x, i)::xs => x :: loop(n - 1, xs.unzip._1)
    }
    loop(nth, list)
  }*/

  //def drop[A](nth: Int, list: List[A]): List[A] = list.zipWithIndex.withFilter(b => (b._2 + 1) % nth != 0).map(_._1)

  def drop[A](nth: Int, list: List[A]): List[A] = for {
    (a, i) <- list.zipWithIndex
    if (i + 1) % nth != 0
  } yield a

  def split[A](length: Int, list: List[A]): (List[A], List[A]) = (list.take(length), list.drop(length))

  //def slice[A](a: Int, b: Int, list: List[A]): List[A] = list.take(b).drop(a)

  def slice[A](a: Int, b: Int, list: List[A]): List[A] = list match {
    //list.take(b).drop(a)
    case x :: xs if a > 0 => slice(a - 1, b - 1, xs)
    case x :: xs if b > 0 => x :: slice(a, b - 1, xs)
    case _ => Nil
  }

  def rotate[A](n: Int, list: List[A]): List[A] = list match {
    case Nil => Nil
    case x :: xs if n > 0 => rotate(n - 1, xs ::: List(x))
    case x :: xs if n < 0 => rotate(n + 1, xs.last :: x :: xs.init)
    case xs if n == 0 => xs
  }

  def removeAt[A](n: Int, list: List[A]): (List[A], A) = {
    val (l1, l2) = list.splitAt(n)
    (l1 ::: l2.tail, l2.head)
  }

  /*def insertAt[A](elem: A, i: Int, list: List[A]): List[A] = {
    val (l1,l2) = list.splitAt(i)
    l1 ::: List(elem) ::: l2
  }*/

  def insertAt[A](elem: A, i: Int, list: List[A]): List[A] = list match {
    case Nil => Nil
    case xs if i == 0 => elem :: xs
    case x :: xs => x :: insertAt(elem, i - 1, xs)
  }

  def range(i: Int, j: Int): List[Int] = {
    //Range(i, j).toList
    if (i == j)
      i :: Nil
    else
      i :: range(i + 1, j)
  }

  def randomSelect[A](n: Int, list: List[A]): List[A] = n match {
    case 0 => Nil
    case _ => val index = Random.nextInt(list.length)
      val (xs, elem) = removeAt(index, list)
      elem :: randomSelect(n - 1, xs)
  }

  def lotto(i: Int, j: Int): List[Int] = randomSelect(i, range(i, j))

  def randomPermute[A](list: List[A]): List[A] = randomSelect(list.length, list)

  def combinations[A](n: Int, list: List[A]): List[List[A]] = {
    def flatMapSub[A,B](list: List[A])(f: List[A] => List[B]): List[B] = list match {
      case Nil => Nil
      case x::xs => f(x::xs) ::: flatMapSub(xs)(f)
    }

    if(n == 0) List(Nil)
    else flatMapSub(list){ s1 =>
      combinations(n - 1, s1.tail).map(s1.head :: _)
    }
  }

}
