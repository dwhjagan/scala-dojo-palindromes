package palindrome

import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: evantill
 * Date: 16/10/13
 * Time: 12:55
 * To change this template use File | Settings | File Templates.
 */
private[palindrome] case class StreamContext[A](elem: A, before: Stream[A], after: Stream[A])

private[palindrome] object StreamContext {
  def apply[A](s: Stream[A]): Stream[StreamContext[A]] = {
    @tailrec
    def addContextRec(rest: Stream[A], before: Stream[A], result: Stream[StreamContext[A]]): Stream[StreamContext[A]] = rest match {
      case h #:: t => {
        val elem: StreamContext[A] = StreamContext(h, before, t)
        addContextRec(t, h +: before, result :+ elem)
      }
      case _ => result
    }
    addContextRec(s, Stream.empty, Stream.empty)
  }
}

