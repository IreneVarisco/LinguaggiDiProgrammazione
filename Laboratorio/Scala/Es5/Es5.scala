import scala.io._
import java.io._

object Es5{
    def main(args: Array[String]): Unit = {
        var file = Source.fromFile("kwic.txt").getLines().flatMap(_.split("\\s+")).toList

        //val words = file.getLines().mkString(" ").split("\\s+").toList
        
        val initialSize = file.size
        for (_ <- 1 to initialSize) {
            var a = file.head
            file = file.tail
            file = file :+ a
            println(file.mkString)
        }
    }
}