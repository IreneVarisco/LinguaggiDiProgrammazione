import scala.io._
import java.io._
object KWIC {
    def main(args: Array[String]) : Unit  = {
        
        if(args.isEmpty)
            throw new IllegalArgumentException

        val file = if(new File(args(0)).exists()) Source.fromFile(args(0)) else null
        val lines = if(file != null) file.getLines().toList.map(_.trim()) else List.empty
        
        val kwicObjects : List[KWICObj] = lines.map(new KWICObj(_)).toList.sorted
        printKwic(lines, kwicObjects)
      
    }

    def printKwic(lines : List[String], kwicObjects: List[KWICObj]) : Unit = {
        def rprintKwic(kwicObjects : List[KWICObj]) : Unit = {
            kwicObjects match {
                case hdO :: tlO => {
                    hdO.sortedWords match {
                        case hd :: tl => {
                            hdO.printKwicAndPopWord(lines.indexOf(hdO.phrase))
                            rprintKwic((hdO :: tlO).sorted)
                        }
                        case Nil => {}
                    }
                }
                case Nil => {
                    println("File not found or cannot KWIC an empty file")
                }
            }
        }
        rprintKwic(kwicObjects)
    }
}

class KWICObj (val phrase: String) extends Ordered[KWICObj]{
    var sortedWords = {
        (
            for{
                s <- phrase.split(" ")
                if(!MinorWords.isMinor(s))
            } yield s
        ).toList.sorted
    }

    def compare(that: KWICObj) : Int  = { 
        if(this.sortedWords.isEmpty) return 1
        if(that.sortedWords.isEmpty) return -1
        this.sortedWords.head.compare(that.sortedWords.head)
    }

    def printKwicAndPopWord(index: Int) = {
        val kwicWord = sortedWords.head
        val indexOfKwicWord = phrase.indexOf(kwicWord)
        val beginRightPart = indexOfKwicWord + kwicWord.length + 1
        var leftPart = phrase.substring(0, indexOfKwicWord) match {
            case p if p.length < 33 => {
                var l = p
                while(l.length < 34) l = " " + l
                l
            }
            case p if p.length > 33 => p.substring(indexOfKwicWord - 34)
            case p => p
        }
        val rightPart = phrase match {
            case p if phrase.length > beginRightPart => {
                val s = p.substring(beginRightPart)
                if(s.length > 40) s.substring(0, 40 - kwicWord.length) else s
            }
            case _ => ""
        }
        println(s"    ${index+1} $leftPart$kwicWord $rightPart")
        sortedWords = sortedWords.tail
    }

    override def toString() : String = s"${phrase.toUpperCase()}:\n [${sortedWords.reduce((acc,s) => acc + "," + s)}]"

}

object MinorWords {
   val words : List[String] = "about" :: "a"::"an"::"the"::"and"::"but"::"or"::"nor"::"so"::"for"::"yet"::
    "at"::"by"::"for"::"in"::"of"::"on"::"to"::"with"::"up"::"down"::"off"::"out"::"over"::"under"::
    "as"::"than"::"if"::"then"::"that"::"which"::"when"::"where"::"why" :: Nil

    def isMinor(s: String) = words.contains(s.toLowerCase())
}



