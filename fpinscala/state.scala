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
  def intDouble(rng: RNG): ((Int,Double), RNG)
  def doubleInt(rng: RNG): ((Double,Int), RNG)
  def double3(rng: RNG): ((Double,Double,Double), RNG)
}

object Main {
  def main(args: Array[String]): Unit = { 
    val rng = SimpleRNG(442)
    println(RNG.double(rng))
  } 
}
