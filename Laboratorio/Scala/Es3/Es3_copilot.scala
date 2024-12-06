import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.{Behaviors, ActorContext}
import scala.concurrent.duration._

object StringReverserSystem {
  // Slave Actor per invertire sottostringhe
  object SlaveProcess {
    sealed trait Command
    case class Reverse(part: String, replyTo: ActorRef[String]) extends Command

    def apply(): Behavior[Command] = Behaviors.receiveMessage {
      case Reverse(part, replyTo) =>
        replyTo ! part.reverse
        Behaviors.same
    }
  }

  // Master Actor per coordinare l'inversione
  object MasterProcess {
    sealed trait Command
    case class ReverseString(input: String, replyTo: ActorRef[String], numParts: Int = 10) extends Command
    private case class PartReversed(part: String, index: Int) extends Command

    def apply(): Behavior[Command] = Behaviors.setup { context =>
      Behaviors.receiveMessage {
        case ReverseString(input, replyTo, numParts) =>
          // Dividi la stringa in parti
          val parts = splitString(input, numParts)
          
          // Crea attori slave
          val slaveActors = (0 until numParts).map(_ => 
            context.spawn(SlaveProcess(), s"slave-${context.self.path.name}")
          )

          // Stato per raccogliere risultati
          var reversedParts = new Array[String](numParts)
          var receivedCount = 0

          // Invia parti agli slave
          parts.zipWithIndex.foreach { case (part, index) =>
            slaveActors(index) ! SlaveProcess.Reverse(part, 
              context.messageAdapter[String](reversed => PartReversed(reversed, index))
            )
          }

          // Comportamento per gestire risposte
          Behaviors.receiveMessage {
            case PartReversed(reversed, index) =>
              reversedParts(index) = reversed
              receivedCount += 1

              if (receivedCount == numParts) {
                // Ricomponi stringa invertita
                val finalReversed = reversedParts.mkString
                replyTo ! finalReversed
                Behaviors.stopped
              } else {
                Behaviors.same
              }
          }
      }
    }

    // Utility per dividere la stringa
    private def splitString(input: String, numParts: Int): Seq[String] = {
      val totalLength = input.length
      val baseLength = totalLength / numParts
      val remainder = totalLength % numParts

      (0 until numParts).map { index =>
        val start = index * baseLength + math.min(index, remainder)
        val end = start + baseLength + (if (index < remainder) 1 else 0)
        input.substring(start, end)
      }
    }
  }

  // Client per effettuare richieste
  object ClientProcess {
    sealed trait Command
    case class ReverseRequest(input: String, numParts: Int = 10) extends Command

    def apply(): Behavior[Command] = Behaviors.setup { context =>
      val masterActor = context.spawn(MasterProcess(), "master")

      Behaviors.receiveMessage {
        case ReverseRequest(input, numParts) =>
          val replyTo = context.messageAdapter[String] { reversed =>
            println(s"Reversed string: $reversed")
            ReverseRequest(input, numParts)
          }

          masterActor ! MasterProcess.ReverseString(input, replyTo, numParts)
          Behaviors.same
      }
    }
  }

  // Metodo main per avviare il sistema
  def main(args: Array[String]): Unit = {
    val system = ActorSystem(ClientProcess(), "StringReverserSystem")
    
    // Esempi di utilizzo
    system.tell(ClientProcess.ReverseRequest("Lunga stringa da invertire con Akka"))
    system.tell(ClientProcess.ReverseRequest("Esempio con numero variabile di parti", 5))
  }
}