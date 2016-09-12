import SplitTreasure._

class SplitTreasureSpec extends BaseSpec {

  "splitGems" when {
    "things go wrong" should {
      "return nothing to split for no gems, regardless of hunters" in {
        splitGems(Seq.empty, 1) shouldBe Seq.empty
        splitGems(Seq.empty, 2) shouldBe Seq.empty
        splitGems(Seq.empty, 3) shouldBe Seq.empty
      }

      "error if there are not enough hunters" in {
        an[IllegalArgumentException] should be thrownBy splitGems(Seq(1, 2, 3), -1)
        an[IllegalArgumentException] should be thrownBy splitGems(Seq(1, 2, 3), 0)
      }

      "error if there are more hunters than gems" in {
        an[IllegalArgumentException] should be thrownBy splitGems(Seq(1, 2, 3), 4)
      }
    }

    "there is a single hunter" should {
      "give them all the gems" in {
        splitGems(Seq(2), 1) shouldBe Seq(Seq(2))
        splitGems(Seq(1, 2), 1) shouldBe Seq(Seq(1, 2))
      }
    }

    "all gems have the same value" should {
      "give a gem to each hunter if they are the same quantity" in {
        splitGems(Seq(1, 1), 2) shouldBe Seq(Seq(1), Seq(1))
        splitGems(Seq(1, 1, 1), 3) shouldBe Seq(Seq(1), Seq(1), Seq(1))
      }

      "split evenly if the gems are a multiple of the hunters" in {
        splitGems(Seq(1, 1, 1, 1, 1, 1), 2) shouldBe Seq(Seq(1, 1, 1), Seq(1, 1, 1))
        splitGems(Seq(1, 1, 1, 1, 1, 1), 3) shouldBe Seq(Seq(1, 1), Seq(1, 1), Seq(1, 1))
      }

      "not split anything if the gems are not a multiple of the hunters" in {
        splitGems(Seq(1, 1, 1), 2) shouldBe Seq.empty
      }
    }

    "gems have different values" should {
      "not split if the gem values are not divisible by the number of hunters" in {
        splitGems(Seq(1, 2), 2) shouldBe Seq.empty
      }

      "split [1, 1, 2] by two hunters as [1, 1] and [2]" in {
        pending
        splitGems(Seq(1, 1, 2), 2) shouldBe Seq(Seq(1, 1), Seq(2))
      }

      "split and respect the order of gems [3, 5, 2] by two hunters as [3, 2] and [5]" in {
        pending
        splitGems(Seq(3, 5, 2), 2) shouldBe Seq(Seq(3, 2), Seq(5))
      }
    }
  }
}
