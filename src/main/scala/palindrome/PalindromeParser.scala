package palindrome

/**
 * Created with IntelliJ IDEA.
 * User: evantill
 * Date: 16/10/13
 * Time: 12:59
 * To change this template use File | Settings | File Templates.
 */
object PalindromeParser {

  case class Palindrome(text: String, index: Int, length: Int)

  object Palindrome {
    val orderedByLengthDesc = Ordering.by[Palindrome, Int](_.length).reverse
  }

  type PalindromeStream = Stream[Palindrome]

  type ParserStream = Stream[StreamContext[CharWithContext]]

  trait ParserFilter {
    def apply(s: PalindromeStream): PalindromeStream
  }

  implicit val defaultParserFilter = new ParserFilter {
    override def apply(s: PalindromeStream) = s.sorted(Palindrome.orderedByLengthDesc).take(3)
  }

  def parse(text: String)(implicit filter: ParserFilter): List[Palindrome] = parse(text.toStream)(filter).toList

  def parse(text: Stream[Char])(implicit filter: ParserFilter): Stream[Palindrome] = {

    val searchStream: ParserStream = StreamContext(CharWithContext(text))

    def findPalindrome(ctx: StreamContext[CharWithContext]): Option[Palindrome] = {
      val zipped = ctx.before.zip(ctx.after)
      val (prefix, suffix) = zipped.takeWhile {
        case (b, a) => b.c == a.c
      }.unzip
      if (prefix.isEmpty) {
        None
      } else {
        val palindromeStream = prefix.reverse ++ ctx.elem #:: suffix
        val text = palindromeStream.map(_.uncompress).mkString("")
        val index = palindromeStream.head.index
        Some(Palindrome(text, index = index, length = text.length))
      }
    }

    filter(searchStream.map(findPalindrome).filter(_.isDefined).map(_.get))
  }
}
