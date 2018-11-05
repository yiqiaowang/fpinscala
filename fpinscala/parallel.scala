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


