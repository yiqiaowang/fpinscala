trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = { 
    val newSeed = (seed *  0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}


object RNG {
  type Rand[A] = State[RNG, A]
  type State[S, +A] = S => (A, S)
  def nonNegativeInt(rng: RNG): (Int, RNG) = { 
    val (n, nextRNG) = rng.nextInt
    (n, nextRNG) match {
      case (a, b) if (a == Int.MinValue) => (Int.MaxValue, nextRNG)
      case _ => if (n < 0) (-n, nextRNG) else (n, nextRNG)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = RNG.nonNegativeInt(rng)
    (n.toDouble / Int.MaxValue, nextRNG)
  }
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (int, nextRNG) = rng.nextInt
    val (double, callerRNG) = RNG.double(nextRNG)
    ((int, double), callerRNG)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (double, nextRNG) = RNG.double(rng)
    val (int, callerRNG) = nextRNG.nextInt
    ((double, int), callerRNG)
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (double1, nextRNG1) = RNG.double(rng)
    val (double2, nextRNG2) = RNG.double(nextRNG1)
    val (double3, nextRNG3) = RNG.double(nextRNG2)
    ((double1, double2, double3), nextRNG3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0)
      (Nil, rng)
    else {
      val (n, nextRNG) = rng.nextInt
      (n :: RNG.ints(count - 1)(nextRNG)._1, nextRNG)
    }
  }

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = 
    map(nonNegativeInt)(i => i - i % 2)

  def doubleMap: Rand[Double] =
    map(nonNegativeInt)((i: Int) => i.toDouble / Int.MaxValue)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rngA) = ra(rng)
      val (b, rngB) = rb(rngA)
      (f(a, b), rngB) 
    }
  
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  def randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  // def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  //   fs.foldRight(rng => (Nil, rng))((a, b) => {r => (a :: )})
  //     def sequenceViaFoldRight[S,A](sas: List[State[S, A]]): State[S, List[A]] =
  //         sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rngA) = f(rng) 
      g(a)(rngA)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if ( i + (n-1) - mod >= 0 ) unit(mod) else nonNegativeLessThan(n)
    })

  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => {b => (f(a), (b))})

  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def rollDie: Rand[Int] = nonNegativeLessThan(6)

  def rollDie2: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

case class State[S, +A](run: S => (A, S)) {
  // def unit  
  // def map
  // def map2
  // def flatMap
  // def sequence

}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
}

object Main {
  def main(args: Array[String]): Unit = { 
    val rng = SimpleRNG(442)
    println(RNG.ints(5)(rng))
  } 
}
