val xs = List(2, 1, 6, 3, 6)
val h = 2

val y = xs.sum / h

xs.sorted.foldLeft(Seq.empty[Seq[Int]]) { (s, n) =>
  if (s.isEmpty)
    Seq(Seq(n))
  else if (s.head.sum + n <= y)
    (s.head :+ n) +: s.tail
  else
    Seq(n) +: s
}

//xs.groupBy(_ % 2)
