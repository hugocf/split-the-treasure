object SplitTreasure {

  def splitGems(gs: Seq[Int], n: Int): Seq[Seq[Int]] = {
    if (gs.nonEmpty) {
      require(n > 0, "must have some hunters, yo!")
    }

    if (gs.isEmpty || gs.sum % n != 0 || gs.max > gs.sum / n )
      Seq.empty
    else {
      val y = gs.sum / n
      groupGemsByValue(Seq(Seq.empty), gs.sorted.reverse, Seq.empty, y) match {
        case (Nil, gr) => Seq.empty
        case (gl, gr) if gr.nonEmpty => Seq.empty
        case (g :: gl, gr) if g.sum != y => Seq.empty
        case (gl, gr) => gl
      }
    }

  }

  def groupGemsByValue(acc: Seq[Seq[Int]], gs: Seq[Int], gr: Seq[Int], y: Int): (Seq[Seq[Int]], Seq[Int]) = {
    gs match {
      case Nil => (acc, gr)
      case g :: s if acc.head.sum == y => if (g <= y) groupGemsByValue(Seq(g) +: acc, s, gr, y) else groupGemsByValue(acc, s, gr :+ g, y)
      case g :: s if acc.head.sum + g == y => groupGemsByValue((acc.head :+ g) +: acc.tail, gr ++ s, Seq.empty, y)
      case g :: s if acc.head.sum + g < y => groupGemsByValue((acc.head :+ g) +: acc.tail, s, gr, y)
      case g :: s => groupGemsByValue(acc, s, gr :+ g, y)
    }
  }

}
