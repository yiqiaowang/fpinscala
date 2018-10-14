package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A] (head: A, tail: List[A]) extends List[A]

object List {
  def sum( ints: List[Int] ): Int = ints match{
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }  

  def product(ds: List[Double]): Double = ds match { 
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(h, t) => h * product(t)
  }

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A] (l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t) } 
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case x => drop(List.tail(l), n-1)
  }

  def dropWhile[A] (l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (f(h)) dropWhile(t, f) else l
  }

  def init[A] (l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Cons(t, Nil)) => List(h)
    case Cons(h, t) => Cons(h, List.init(t))
  }

  def foldRight[A, B] (as: List[A], z: B) (f: (A, B) => B) : B =
    as match {
      case Nil => z
      case Cons(h, t) => f(h, List.foldRight(t, z)(f))
    }

  def sum2(ns: List[Int]): Int = List.foldRight(ns, 0)((x, y) => x + y)
  def product2(ns: List[Int]): Int = List.foldRight(ns, 1)((x, y) => x * y)
  def length[A] (as: List[A]): Int = List.foldRight(as, 0)((x, y) => 1 + y)

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def last[A] (as: List[A]): A = as match {
    case Nil => sys.error("wat")
    case Cons(x, Nil) => x
    case Cons(_, xs) => List.last(xs)
  }


  @annotation.tailrec
  def foldLeft[A, B] (as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => List.foldLeft(t, f(z, h))(f)
    }

  def sum3(ns: List[Int]): Int = List.foldLeft(ns, 0)(_ + _)
  def product3(ns: List[Int]): Int = List.foldLeft(ns, 1)((x, y) => x * y)
  def length2[A](l: List[A]): Int = List.foldLeft(l, 0)((x, y) => 1 + x)

  def append[A](l: List[A], r: List[A]): List[A] = List.foldRight(l, r)(Cons(_, _))
  def concat[A](ls: List[List[A]]): List[A] = List.foldRight(ls, Nil: List[A])(append)

  def addOne(ns: List[Int]): List[Int] = ns match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, List.addOne(t))
  }

  def toString(ns: List[Double]): List[String] = ns match {
    case Nil => Nil
    case Cons(h, t) => Cons(h.toString(), List.toString(t))
  }

  def map[A, B](ns: List[A])(f: A => B): List[B] = ns match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), List.map(t)(f)) } 
  def filter[A](ns: List[A])(f: A => Boolean): List[A] = ns match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) Cons(h, List.filter(t)(f)) else List.filter(t)(f)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => List.append(f(h), List.flatMap(t)(f))
  }

  def filter2[A](ns: List[A])(f: A => Boolean): List[A] = {
    List.flatMap(ns)(a => if (f(a)) List(a) else Nil)
  }

  def zipWith[A, B, C](xs: List[A], ys: List[B])(f: (A, B) => C): List[C] = (xs, ys) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(xh, xt), Cons(yh, yt)) => Cons(f(xh, yh), zipWith(xt, yt)(f))
  }

  def matchSublist[A](main: List[A], sub: List[A]): Boolean = (main, sub) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(x, y)) => h == x && matchSublist(t, y)
    case _ => false
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Cons(h, t), sub) => matchSublist(sup, sub) || hasSubsequence(t, sub)
    case _ => false
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    var l1 = List(1,2,3,4)
    var l2 = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    var l3 = List(1.1, 2.2, 3.3, 4.4)
    
    // println(List.hasSubsequence(l1, List(2,3)))
    println(List.hasSubsequence(List(1,2,3,4), List(3,4)))
    // println(List.hasSubsequence(l1, List(1,2)))
    // println(List.hasSubsequence(l1, List(2)))
    // println(List.hasSubsequence(l1, List(1,3)))
  }
}
