object SplitTreasure {

  def splitGems(gs: Seq[Int], n: Int): Seq[Seq[Int]] = {
    if (gs.nonEmpty) {
      require(n > 0, "must have some hunters, yo!")
      require(n <= gs.length, "eeeei… too many hunters, bro!")
    }

    if (gs.isEmpty)
      Seq.empty
    else if (gs.forall(_ == gs.head)) {
      if (gs.length % n != 0)
        Seq.empty
      else
        gs.grouped(gs.length / n).toSeq
    } else
      gs.groupBy()
  }

}
