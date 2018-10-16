sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] = {
    this.map(f).getOrElse(None)
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(b) => b
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this.map(Some(_)).getOrElse(ob)
  }
  def filter(f: A => Boolean): Option[A] = {
    this.flatMap(a => if (f(a)) Some(a) else None)
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }
  
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match { 
    case (Some(v1), Some(v2)) => Some(f(v1, v2))
    case _ => None
  }

  def map2_better[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = { 
    a.flatMap(v1 => b.map(v2 => f(v1, v2)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap(x => sequence(t).map(x :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => f(h).flatMap(y => traverse(t)(f).map(y :: _))
  }

  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse2(t)(f))((oh, ot) => oh :: ot)
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
