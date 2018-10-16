sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): List[A] = (this, n) match {
    case (Cons(h, t), x) if (x > 0) => h() :: t().take(n-1) 
    case _ => Nil
  }

  def head: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case Empty => None
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

  def mapUnfold[B] (f: A => B): Stream[B] =
    Stream.unfold(this)({
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    })

  def takeUnfold (n: Int): List[A] =
    Stream.unfold((this, n))({
      case (Cons(h, t), n) if (n > 0) => Some((h(), (t(), n-1)))
      case _ => None
    }).toList

  def takeWhileUnfold(f: A => Boolean): List[A] =
    Stream.unfold(this)({
      case Cons(h, t) if (f(h())) => Some((h(), t()))
      case _ => None
    }).toList

  def zipWith[B, C](ys: Stream[B])(f: (A, B) => C): List[C] =
    Stream.unfold((this, ys))({
      case (Cons(h, t), Cons(hh, tt)) => Some((f(h(), hh()), (t(), tt())))
      case _ => None
    }).toList

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, s2))( {
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
      case (Cons(h, t), Cons(hh, tt)) => Some((Some(h()), Some(hh())), (t(), tt()))
      case _ => None
    } )

  def startsWith[A](s: Stream[A]): Boolean = (this, s) match {
    case (_, Empty) => true
    case (Cons(h, t), Cons(x, y)) => h() == x() && t().startsWith(y())
    case _ => false
  }

  // How ghetto is this...
  def tails: Stream[Stream[A]] = {
    Stream.unfold(
      this.head match {
        case None => Empty
        case Some(a) => Stream.cons(a, this)
        }) ({
          case Empty => None
          case Cons(h, t) => Some((t(), t()))
        })
  }


  // def hasSubsequence[A](s: Stream[A]): Boolean =
  //   tails exists (_ startsWith s)

  // Not exactly correct as it doesn't handle what to do on the empty stream
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = 
    Stream.unfold(this)({
      case Empty => None
      case Cons(h, t) => Some((f(h(), t().foldRight(z)(f)), t()))
    })

  // def foldRight[B] (z: => B) (f: (A, => B) => B): B = this match {
  //   case Cons(h, t) => f(h(), t().foldRight(z)(f))
  //   case _ => z
  // }
  // def scanRight2[B](z: => B)(f: (A, => B) => B): Stream[B] = 
  //   this.foldRight((z, Stream.cons(z)))((a, b) =>  )
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
    println(Stream(1,2,3).scanRight(0)(_ + _).toList)
  }
}
