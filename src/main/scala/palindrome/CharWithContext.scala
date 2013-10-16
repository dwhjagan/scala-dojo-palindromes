package palindrome

import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: evantill
 * Date: 16/10/13
 * Time: 12:54
 * To change this template use File | Settings | File Templates.
 */
private[palindrome] case class CharWithContext(c: Char, index: Int, length: Int) {

  def uncompress: String = Array.fill(length)(c).mkString("")

  def sameChar(other: CharWithContext): Boolean = c == other.c

  def compressWith(other: CharWithContext): CharWithContext = {
    assume(other.c == this.c)
    copy(length = this.length + other.length)
  }
}

private[palindrome] object CharWithContext {

  def apply(text: Stream[Char]): Stream[CharWithContext] = {
    def withContext(text: Stream[Char]): Stream[CharWithContext] = {
      val contextStream = Stream.from(0) zip Stream.continually(1)
      (text zip contextStream).map {
        case (c, (index, length)) => CharWithContext(c, index, length)
      }
    }

    def compress(text: Stream[CharWithContext]): Stream[CharWithContext] = {
      @tailrec
      def compress(last: CharWithContext, uncompressed: Stream[CharWithContext], compressed: Stream[CharWithContext]): Stream[CharWithContext] = uncompressed match {
        case h #:: t => {
          if (last.sameChar(h)) compress(last.compressWith(h), t, compressed)
          else compress(h, t, compressed :+ last)
        }
        case _ => {
          compressed :+ last
        }
      }
      text match {
        case h #:: t => compress(h, t, Stream.empty)
        case _ => Stream.empty
      }
    }

    compress(withContext(text))
  }

}
