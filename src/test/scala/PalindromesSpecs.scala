import org.specs2.mutable._
import Palindromes._

class PalindromesSpecs extends Specification {

  "the 'sqrrqabccbatudefggfedvwhijkllkjihxymnnmzpop' strings'" should {

    "contains palindrome hijkllkjih at index 23 and length 10" in {

      val raw: Stream[Char] = "sqrrqabccbatudefggfedvwhijkllkjihxymnnmzpop".toStream

      val expected = Seq(
        Palindrome("hijkllkjih",index=23, length= 10),
        Palindrome("defggfed",index=13, length= 8),
        Palindrome("abccba",index=5 ,length= 6)
      )

      val results=Palindromes.find(raw)

      results.foreach(_.pp)

      results must haveSize(3)

      results must containTheSameElementsAs(expected)

      results must beSorted(Palindromes.orderedByLengthDesc)
    }
  }
}
