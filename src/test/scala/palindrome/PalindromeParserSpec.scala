package palindrome

import org.specs2.mutable._
import PalindromeParser._

class PalindromeParserSpec extends Specification {

  "the 'sqrrqabccbatudefggfedvwhijkllkjihxymnnmzpop' strings'" should {

    "contains palindrome hijkllkjih at index 23 and length 10" in {

      val raw = "sqrrqabccbatudefggfedvwhijkllkjihxymnnmzpop"

      val expected = Seq(
        Palindrome("hijkllkjih", index = 23, length = 10),
        Palindrome("defggfed", index = 13, length = 8),
        Palindrome("abccba", index = 5, length = 6)
      )

      val results = PalindromeParser.parse(raw)

      results.foreach(_.pp)

      results must haveSize(3)

      results must containTheSameElementsAs(expected)

      results must beSorted(Palindrome.orderedByLengthDesc)
    }
  }
}
