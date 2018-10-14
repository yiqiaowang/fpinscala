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
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
