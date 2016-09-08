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
        splitGems(Seq(1, 2), 1) shouldBe Seq(Seq(2, 1))
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
        pending
        splitGems(Seq(3, 5, 2), 2) shouldBe Seq(Seq(3, 2), Seq(5))
      }
    }
  }

  "groupGemsByValue" when {

    "there is only 1 gem" should {
      "return the gem if the group value is greater or equal to the gem value and 1 empty set" in {
        groupGemsByValue(Seq(Seq.empty), Seq(7), Seq.empty, 7) shouldBe (Seq(Seq(7)), Seq.empty)
        groupGemsByValue(Seq(Seq.empty), Seq(7), Seq.empty, 10) shouldBe (Seq(Seq(7)), Seq.empty)
      }

      "return nothing if the group value is less to the gem value and return the remainder set with the initial gem" in {
        groupGemsByValue(Seq(Seq.empty), Seq(7), Seq.empty, 5) shouldBe (Seq(Seq.empty), Seq(7))
      }
    }

    "there are only 2 gems" should {
      "return the 2 gems if the group value is greater or equal to the sum of the gems values and 1 empty set" in {
        groupGemsByValue(Seq(Seq.empty), Seq(3, 4), Seq.empty, 7) shouldBe (Seq(Seq(3, 4)), Seq.empty)
        groupGemsByValue(Seq(Seq.empty), Seq(3, 4), Seq.empty, 10) shouldBe (Seq(Seq(3, 4)), Seq.empty)
      }

      "return nothing if the group value is less to any gem value and return the remainder set with the initials gems" in {
        groupGemsByValue(Seq(Seq.empty), Seq(3, 4), Seq.empty, 2) shouldBe (Seq(Seq.empty), Seq(3, 4))
      }

      "return the gem with the value lower than the group value if in the 2 gems 1 have lower value than the group value and the other has a value greater than the group value independently of the order of the gems, it should also return the remainder set with the gem" in {
        groupGemsByValue(Seq(Seq.empty), Seq(3, 5), Seq.empty, 4) shouldBe (Seq(Seq(3)), Seq(5))
        groupGemsByValue(Seq(Seq.empty), Seq(5, 3), Seq.empty, 4) shouldBe (Seq(Seq(3)), Seq(5))
      }
    }

    "there are only 3 gems" should {
      "return the 3 gems if the group value is greater or equal to the sum of the gems values and 1 empty set" in {
        groupGemsByValue(Seq(Seq.empty), Seq(4, 3, 3), Seq.empty, 10) shouldBe (Seq(Seq(4, 3, 3)), Seq.empty)
      }

      "return nothing if the group value is less to any gem value and return the remainder set with the initials gems" in {
        groupGemsByValue(Seq(Seq.empty), Seq(4, 3, 3), Seq.empty, 2) shouldBe (Seq(Seq.empty), Seq(4, 3, 3))
      }

      "return the 2 gems with the value lower than the group value if in the 2 gems 2 have lower value than the group value and the other has a value greater than the group value, it should also return the remainder set with the gem" in {
        groupGemsByValue(Seq(Seq.empty), Seq(10, 3, 2), Seq.empty, 5) shouldBe (Seq(Seq(3, 2)), Seq(10))
      }

      "return the 1st and the 3rd gems with the value equal to the group value and return the remainder set with the gem" in {
        groupGemsByValue(Seq(Seq.empty), Seq(10, 3, 2), Seq.empty, 12) shouldBe (Seq(Seq(3), Seq(10, 2)), Seq.empty)
        groupGemsByValue(Seq(Seq.empty), Seq(10, 23, 2), Seq.empty, 12) shouldBe (Seq(Seq(10, 2)), Seq(23))
      }
    }

    "there are several gems" should {
      "return all the gems if the group value is greater or equal to the sum of the gems values and 1 empty set" in {
        groupGemsByValue(Seq(Seq.empty), Seq(4, 3, 3, 2), Seq.empty, 12) shouldBe (Seq(Seq(4, 3, 3, 2)), Seq.empty)
        groupGemsByValue(Seq(Seq.empty), Seq(4, 3, 3, 2), Seq.empty, 14) shouldBe (Seq(Seq(4, 3, 3, 2)), Seq.empty)
      }

      "return nothing if the group value is less to any gem value and return the remainder set with the initials gems" in {
        groupGemsByValue(Seq(Seq.empty), Seq(10, 4, 3, 3), Seq.empty, 2) shouldBe (Seq(Seq.empty), Seq(10, 4, 3, 3))
      }

      "return the initial gems that sum up to the group value and return the unused gem on the remainder set" in {
        groupGemsByValue(Seq(Seq.empty), Seq(10, 3, 2), Seq.empty, 12) shouldBe (Seq(Seq(3), Seq(10, 2)), Seq.empty)
      }

    }

  }

}

