// Kevin Manca
// Linguaggi di Programmazione
// Esame (parziale) Scala - 27/03/2023

import scala.util.parsing.combinator._
import java.io.File
import java.nio.file._
import scala.util.Try

class LogLangParser extends JavaTokenParsers {

  // Task
  def tasks = rep1(task)

  private def task = ( "task" ~> taskName <~ "{" ) ~ ( rep1(operation) <~ "}" ) ^^ {
    case name~results => {
      println(f"Task $name")
      results.zipWithIndex.foreach{case (r,i) => println(f"  [op${i+1}] $r")}
    }
  }

  private def taskName : Parser[String]  = ident
  private def operation: Parser[Boolean] = remove | rename | merge | backup

  private def remove: Parser[Boolean] = "remove" ~> stringLiteral ^^ {
    p => removeFile( unquoted(p) )
  }

  private def rename: Parser[Boolean] = "rename" ~> stringLiteral ~ stringLiteral ^^ {
    case f ~ t => renameFile(unquoted(f), unquoted(t))
  }

  private def merge : Parser[Boolean] = "merge" ~> stringLiteral ~ stringLiteral ~ stringLiteral ^^ {
    case p1 ~ p2 ~ r => mergeFiles( unquoted(p1), unquoted(p2), unquoted(r) )
  }

  private def backup: Parser[Boolean] = "backup" ~> stringLiteral ~ stringLiteral ^^ {
    case p ~ b => backupFile( unquoted(p), unquoted(b) )
  }

  // remove quotes from paths
  private def unquoted(str: String) = str.substring(1, str.length - 1)

  // Rename
  private def renameFile(name: String, newName: String) =
    Try( new File(from).renameTo(new File(to)) ).getOrElse(false)

  // Remove
  private def removeFile(path: String) =
    Try( new File(path).delete() ).getOrElse(false)

  // Backup
  private def backupFile(path: String, backupPath: String) = {
    try {
      Files.copy(new File(path).toPath, new File(bPath).toPath)
      true
    } catch {
      case e: Throwable => false
    }
  }

  // Merge
  private def mergeFiles(p1: String, p2: String, r: String) = {
    try {
      val c1 = io.Source.fromFile(p1).mkString
      val c2 = io.Source.fromFile(p2).mkString
      Files.writeString( new File(r).toPath, c1++c2, StandardOpenOption.CREATE_NEW )
      true
    } catch {
      case e: Throwable => false
    }
  }
}

// Main
object LogLangParser {
  def main(args: Array[String]): Unit = {
    val p = new LogLangParser
    p.parseAll( p.tasks, io.Source.fromFile(args(0)).reader() ) match {
      case p.Success(a, b) => ()
      case x => println(x.toString)
    }
  }
}
