object SplitTreasure {

  def splitGems(gs: Seq[Int], n: Int): Seq[Seq[Int]] = {
    if (gs.nonEmpty) {
      require(n > 0, "must have some hunters, yo!")
      require(n <= gs.length, "eeeei… too many hunters, bro!")
    }

    if (gs.isEmpty || gs.sum % n != 0 || gs.max > gs.sum / n )
      Seq.empty
    else
      gs.grouped(gs.length / n).toSeq
  }

}
