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

      "return nothing if there are more hunters than gems" in {
        splitGems(Seq(1, 2, 3), 4) shouldBe Seq.empty
      }

      "return nothing if the sum of gem values is not divisible by hunters" in {
        splitGems(Seq(1, 2), 2) shouldBe Seq.empty
        splitGems(Seq(2, 3, 4), 2) shouldBe Seq.empty
      }

      "return nothing if the biggest gem value is greater than the gem value per hunter" in {
        splitGems(Seq(2, 3, 4), 3) shouldBe Seq.empty
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

      "not split anything if the number of gems are not a multiple of the hunters" in {
        splitGems(Seq(1, 1, 1), 2) shouldBe Seq.empty
      }
    }

    "gems have different values" should {
      "split [1, 1, 2] by two hunters as [1, 1] and [2]" in {
        splitGems(Seq(1, 1, 2), 2) shouldBe Seq(Seq(1, 1), Seq(2))
      }

      "split and respect the order of gems [3, 5, 2] by two hunters as [3, 2] and [5]" in {
        splitGems(Seq(3, 5, 2), 2) shouldBe Seq(Seq(3, 2), Seq(5))
      }

      "pass given examples" in {
        splitGems(Seq(4, 4, 4), 3) shouldBe Seq(Seq(4), Seq(4), Seq(4))
        splitGems(Seq(4, 4, 4), 1) shouldBe Seq(Seq(4, 4, 4))
        splitGems(Seq(4, 4, 4), 2) shouldBe Seq.empty
        splitGems(Seq(27, 7, 20), 2) shouldBe Seq(Seq(27), Seq(7, 20))
        splitGems(Seq(27, 7, 20), 3) shouldBe Seq.empty
        splitGems(Seq(6, 3, 2, 4, 1), 2) shouldBe Seq(Seq(6, 2), Seq(3, 4, 1))
        splitGems(Seq(6, 3, 2, 4, 1), 3) shouldBe Seq.empty
        splitGems(Seq(6, 3, 2, 4, 1), 4) shouldBe Seq.empty
        splitGems(Seq(3, 2, 7, 7, 14, 5, 3, 4, 9, 2), 4) shouldBe Seq(Seq(3, 2, 7, 2), Seq(7, 3, 4), Seq(14), Seq(5, 9))
      }

      "answer bonus question" in {
        splitGems(Seq(3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2), 4) shouldBe Seq(Seq(3, 2, 2), Seq(3, 2, 2), Seq(3, 2, 2), Seq(3, 2, 2))
      }
    }
  }
}

