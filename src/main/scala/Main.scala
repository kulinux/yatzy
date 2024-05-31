enum YatzyCategory:
  case Chance
  case Yatzy
  case OnesTwosThreesFoursFivesSixes(dice: Dice)
  case Pair
  case TwoPair

enum Dice(val value: Int):
  case One extends Dice(1)
  case Two extends Dice(2)
  case Three extends Dice(3)
  case Four extends Dice(4)
  case Five extends Dice(5)
  case Six extends Dice(6)

case class Hand(
    one: Dice,
    two: Dice,
    three: Dice,
    four: Dice,
    five: Dice
)

object YatzyGame {

  extension (self: Hand)
    private def all(): Seq[Dice] =
      Seq(self.one, self.two, self.three, self.four, self.five)

  extension (self: Seq[Dice])
    private def sumValues(): Int = self.map(_.value).sum

  def score(hand: Hand, category: YatzyCategory): Int = {
    val all = hand.all()
    val pairs =
      all.groupBy(_.value).filter((_, v) => v.length >= 2)
    category match
      case YatzyCategory.Chance => all.sumValues()
      case YatzyCategory.Yatzy  => 50
      case YatzyCategory.OnesTwosThreesFoursFivesSixes(dice) =>
        all.filter(_ == dice).sumValues()
      case YatzyCategory.Pair => {
        if (pairs.isEmpty) return 0
        val max = pairs.keySet.max
        return 2 * max
      }
      case YatzyCategory.TwoPair => {
        if (pairs.size < 2) return 0
        val sorted = pairs.keySet.toList.sorted.reverse
        return 2 * sorted.head + 2 * sorted.tail.head
      }
  }
}
