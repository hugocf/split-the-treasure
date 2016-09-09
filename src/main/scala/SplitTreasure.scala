object SplitTreasure {

  def splitGems(gs: Seq[Int], n: Int): Seq[Seq[Int]] = {
    if (gs.nonEmpty) {
      require(n > 0, "must have some hunters, yo!")
    }

    if (gs.isEmpty || gs.sum % n != 0 || gs.max > gs.sum / n)
      Seq.empty
    else {
      val y = gs.sum / n
      gs.foldLeft(Seq(Seq.empty): Seq[Seq[Int]])((acc, g) => splitGemsRec(acc, g, gs, n, y)) match {
        case acc => if (acc.length == n && acc.head.sum == y) acc.reverse else Seq.empty
      }

    }
  }

  def splitGemsRec(acc: Seq[Seq[Int]], g: Int, gs: Seq[Int], n: Int, y: Int): Seq[Seq[Int]] = {
    val s = acc.head.sum

    if (acc.length == n && s == y || s + g > y && s != y)
      acc
    else {
      val gr = gs diff Seq(g)
      gr.foldLeft(if (s == y) Seq(g) +: acc else (acc.head :+ g) +: acc.tail)((acc, g) => splitGemsRec(acc, g, gr, n, y)) match {
        case accNew => if (accNew.length == n && accNew.head.sum == y) accNew else acc
      }
    }
  }

}
