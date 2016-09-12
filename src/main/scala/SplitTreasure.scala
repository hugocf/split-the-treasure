object SplitTreasure {

  def splitGems(gs: Seq[Int], n: Int): Seq[Seq[Int]] = {
    def findBucket(gem: Int, buckets: Seq[Seq[Int]], max: Int): Seq[Seq[Int]] =
      buckets match {
        case h :: t if h.sum + gem <= max => t :+ (h :+ gem)
        case h :: t if h.sum + gem > max => findBucket(gem, t :+ h, max)
      }

    if (gs.nonEmpty) {
      require(n > 0, "must have some hunters, yo!")
      require(n <= gs.length, "eeeei… too many hunters, bro!")
    }

    if (gs.isEmpty || gs.sum % n != 0 || gs.max > gs.sum / n )
      Seq.empty
    else if (gs.forall(_ == gs.head))
      gs.grouped(gs.length / n).toSeq
    else {
      val buckets = Seq.fill(n)(Seq.empty[Int])
      gs.sorted.foldRight(buckets)((g, r) => findBucket(g, r, gs.sum / n))
    }
  }

}
