sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): List[A] = (this, n) match {
    case (Cons(h, t), x) if (x > 0) => h() :: t().take(n-1) 
    case _ => Nil
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (p(h())) Cons(h, () => t().takeWhile(p)) else Empty
  }

  def foldRight[B] (z: => B) (f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean =
    this.foldRight(false)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] =
    this.foldRight(Stream.empty[A])((a, b) => if (p(a)) Cons(() => a, () => b) else Stream.empty[A])

  // def headOption
  //

  def map[B](f: A => B): Stream[B] = 
    this.foldRight(Stream.empty[B])((a,b) => Stream.cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    this.foldRight(Stream.empty[A])((a, b) => if (f(a)) Stream.cons(a, b) else b)

  def append[A](s: => Stream[A]): Stream[A] =
    this.foldRight(s)((a, b) => b)

  def flatMap[B](f: A => Stream[B]) = 
    this.foldRight(Stream.empty[A])((a, b) => f(a).append(b))
}

case object Empty extends Stream[Nothing]
// The parameters to the Cons case class constructor must be "forced" thunks
case class Cons[+A] (h:() => A, t:() => Stream[A]) extends Stream[A]

object Stream {
  // Remove the need for explicit thunking
  def cons[A] (hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
