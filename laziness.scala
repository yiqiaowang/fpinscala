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

  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n+1))

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

  val fibs: Stream[Int] = {
    def helper(a:Int, b:Int): Stream[Int] = Stream.cons(a, helper(b, a + b))
    helper(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
  }

  val fibs2: Stream[Int] =
    Stream.unfold((0, 1))({case(a: Int, b: Int) => Some((a, (b, a + b)))})

  def from(n: Int): Stream[Int] = 
    Stream.unfold(n)((n: Int) => Some((n, n+1)))

  def constant(n: Int): Stream[Int] =
    Stream.unfold(n)(n => Some((n, n)))

  val ones: Stream[Int] =
    Stream.unfold(1)(n => Some((1, 1)))
}


object Main {
  def main(args: Array[String]): Unit = {
    println(Stream.from(1).take(5))
  }
}
