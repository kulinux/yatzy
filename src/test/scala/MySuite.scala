import Dice.*
import YatzyCategory.*

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should._

class MySuite extends AnyFreeSpec with Matchers {
  "Chance" - {
    "should score should be the sum of all dice" in {

      val dices = Hand(
        One,
        One,
        Three,
        Three,
        Six
      )

      val actual: Int = YatzyGame.score(dices, Chance)

      actual.shouldBe(14)

    }
  }

  "Yatzy" - {
    "should score 50" in {
      val dices = Hand(
        One,
        One,
        Three,
        Three,
        Six
      )

      val actual: Int = YatzyGame.score(dices, Yatzy)

      actual.shouldBe(50)
    }
  }

  "Ones, Twos, Threes, Fours, Fives, Sixes" - {
    "fours" in {
      val dices = Hand(
        One,
        One,
        Two,
        Four,
        Four
      )

      val actual: Int =
        YatzyGame.score(dices, OnesTwosThreesFoursFivesSixes(Four))

      actual.shouldBe(8)
    }
  }

  "Pair" - {
    "should be 0 if no pair" in {
      val dices = Hand(
        One,
        Two,
        Three,
        Four,
        Five
      )

      val actual: Int =
        YatzyGame.score(dices, Pair)

      actual.shouldBe(0)
    }
  }

  "should be 0 if one pair" in {
    val dices = Hand(
      One,
      One,
      Three,
      Four,
      Five
    )

    val actual: Int =
      YatzyGame.score(dices, Pair)

    actual.shouldBe(2)
  }

  "scores the sum of the two highest matching dice, two four" in {
    val dices = Hand(
      Three,
      Three,
      Three,
      Four,
      Four
    )

    val actual: Int =
      YatzyGame.score(dices, Pair)

    actual.shouldBe(8)
  }

  "scores the sum of the two highest matching dice, two threes" in {
    val dices = Hand(
      Three,
      Three,
      Three,
      Three,
      Four
    )

    val actual: Int =
      YatzyGame.score(dices, Pair)

    actual.shouldBe(6)
  }

  "Two Pairs" - {
    "If there are one pairs of dice with the same number, the player scores 0" in {
      val dices = Hand(
        Three,
        Three,
        Four,
        Five,
        One
      )

      val actual: Int =
        YatzyGame.score(dices, TwoPair)

      actual.shouldBe(0)
    }

    "If there are two pairs of dice with the same number, the player scores the sum of these dice" in {
      val dices = Hand(
        Three,
        Three,
        Four,
        Four,
        One
      )

      val actual: Int =
        YatzyGame.score(dices, TwoPair)

      actual.shouldBe(14)
    }

  }

}
