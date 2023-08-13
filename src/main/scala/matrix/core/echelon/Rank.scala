package matrix.core.echelon

object Rank:
  opaque type RankCounter = Int
  object RankCounter:
    val Empty: RankCounter = 0
    val One: RankCounter   = 1
    extension (counter: RankCounter)
      def increment: RankCounter = counter + 1
      def get: Int               = counter
