// sealed trait Par[+A] {
//   def unit[A](a: => A): Par[A]
//   def get[A](a: => Par[A]): A
//   def run[A](a: => Par[A]): A
//   def fork[A](a: => Par[A]): Par[A]
//   def lazyUnit[A](a: => A): Par[A] = fort(unit(a))
//   def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]
// }

// object Parallel {
//   def sum(ints: Seq[Int]): Int =
//     ints.foldLeft(0)(_ + _)

//   def sum_old(ints: IndexedSeq[Int]): Int =
//     if (ints.size <= 1)
//       ints.headOption getOrElse 0
//     else {
//       val (l, r) = ints.splitAt(ints.length/2)
//       sum_old(l) + sum_old(r)
//     }

//   def sum(ints: IndexedSeq[Int]): Int =
//     if (ints.size <= 1)
//       ints.headOption getOrElse 0
//     else {
//       val (l, r) = ints.splitAt(ints.length/2)
//       val sumL: Par[Int] = Par.unit(sum(l))
//       val sumR: Par[Int] = Par.unit(sum(r))
//       Par.get(sumL) + Par.get(sumR)
//     }
// }


/**
 *
 * Signature for Par.map2
 * def Par.map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]
 *
 * */


/*              Exercise 7.2               */


object Par {
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean) : Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  // Exercise 7.4
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  // Exercise 7.5
  // idea: use map2 and foldLeft
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldLeft[Par[List[A]]](unit(List()))((a, b) => map2(a, b)((x, y) => x ++ y)) 

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    parMap(as)(a => if (f(a)) a else List())
  }
}

