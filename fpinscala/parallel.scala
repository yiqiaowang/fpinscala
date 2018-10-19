object Parallel {
  def sum(ints: Seq[Int]): Int =
    ints.foldLeft(0)((a, b) => a + b)

  def sum_dc(ints: IndexedSeq[Int]): Int = {
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2) 
      sum_dc(l) + sum_dc(r)
    }
  }


  def sum_par(ints: IndexedSeq[Int]): Int = {
    if (ints.size <= 1)
      ints headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val sumL: Par[Int] = Par.unit(sum(l))
      val sumR: Par[Int] = Par.unit(sum(r))
      Par.get(sumL) + Par.get(sumR)
    }
  }
  // unit creates the parallel computation
  // get executes the parallel computation
}
