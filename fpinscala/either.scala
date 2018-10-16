sealed trait Either[+E, +A] {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list is??!!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y:Int): Either[Exception, Int] = {
    try Right(x/y)
    catch { case e: Exception => Left(e) }
  }

  def Try[A] (a: => A): Either[Exception, A] = {
    try Right(a)
    catch { case e: Exception => Left(e) }
  }

  def map[B] (f: A => B): Either [E, B] = this match { 
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B] (f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A] (b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(b) => Right(b)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (b, this) match {
    case (Right(b), Right(a)) => Right(f(a, b))
    case (Left(e),_) => Left(e)
    case (_, Left(e)) => Left(e)
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List [A]] = es match {
    case Left(e) :: _ => Left(e) 
    case h :: t => h.flatMap(a => sequence(t).map(x => a :: x))
    case Nil => Right(Nil)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t => f(h).map2(traverse(t)(f))((eh, et) => eh :: et)
  }
}
case class Left[+E] (value: E) extends Either[E, Nothing]
case class Right[+A] (value: A) extends Either[Nothing, A]
