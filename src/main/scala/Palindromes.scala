import scala._
import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: evantill
 * Date: 15/10/13
 * Time: 14:37
 * To change this template use File | Settings | File Templates.
 */
object Palindromes {

  case class Palindrome(text:String,index:Int,length:Int)

  case class Item(c:Char,pos:Int,size:Int) {
    def uncompress : String = Array.fill(size)(c).mkString("")
  }

  case class Context(item:Item,before:Stream[Item],after:Stream[Item]){

    def palindrome: Option[Palindrome] ={
      val zipped =before.zip(after)
      val (prefix,suffix) =zipped.takeWhile{case (b,a)=>b.c==a.c}.unzip
      if(prefix.isEmpty){
        None
      }else {
        val palindromeStream =prefix.reverse ++ item #:: suffix
        val text = palindromeStream.map(_.uncompress).mkString("")
        val index =  palindromeStream.head.pos
        Some(Palindrome(text,index=index,length=text.length))
      }
    }

  }

  def find(raw:String):List[Palindrome]={
    find(raw.toStream).toList
  }

  def find(from:Stream[Char]):Stream[Palindrome] =findInContext(addContext(compress(items(from))))

  private def findInContext(from:Stream[Context]):Stream[Palindrome]={
    from.map(_.palindrome).filter(_.isDefined).map(_.get).sorted(orderedByLengthDesc).take(3)
  }

  private def items(from:Stream[Char]):Stream[Item]=from.zipWithIndex.map{case (c,pos)=>Item(c,pos,1)}

  private def compress(from:Stream[Item]):Stream[Item]={

    @tailrec
    def comp(compressed:Stream[Item],uncompressed:Stream[Item],last:Item):Stream[Item]= uncompressed match {
      case h #:: t => {
        if(h.c == last.c) comp(compressed,t,last.copy(size=last.size+h.size))
        else comp(compressed :+ last,t,h)
      }
      case _ => compressed :+ last
    }

    if(from.isEmpty) Stream.empty
    else comp(Stream.empty,from.tail,from.head)

  }

  private def addContext(s:Stream[Item]):Stream[Context]={
    @tailrec
    def addCtx(rest:Stream[Item], before:Stream[Item],result:Stream[Context]):Stream[Context]= rest match {
      case h #:: t => {
        val elem=Context(h,before,t)
        addCtx(t,h +: before, result :+ elem )
      }
      case _ => result
    }
    addCtx(s,Stream.empty,Stream.empty)
  }

  val orderedByLengthDesc: Ordering[Palindrome] = Ordering.by[Palindrome,Int](_.length).reverse
}
