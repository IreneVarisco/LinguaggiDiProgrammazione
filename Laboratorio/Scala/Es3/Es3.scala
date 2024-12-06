import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.{Behaviors, StashBuffer, TimerScheduler}

object Es3 {

  object MasterProcess {
    sealed trait Command
    case class long_reverse_string(input: String, replyTo: ActorRef[String]) extends Command
    case class ReversedPart(part: String) extends Command


    def apply(): Behavior[Command] = Behaviors.setup { context =>
      Behaviors.receiveMessage {
        case long_reverse_string(ls, replyTo) =>
          val grouped = ls.grouped(10).toList

          val slaveActors = grouped.indices.map { index =>
            context.spawn(SlaveProcess(), s"slave-$index")
          }

          def collectingBehavior(
            remainingParts: List[String], 
            collectedParts: List[String]
          ): Behavior[Command] = Behaviors.receiveMessage {
            case ReversedPart(part) =>
              val newCollectedParts = part :: collectedParts
              
              if (newCollectedParts.size == grouped.size) {
                // All parts reversed, combine and send back
                val finalReversed = newCollectedParts.reverse.mkString
                replyTo ! finalReversed
                Behaviors.stopped
              } else {
                // Continue collecting
                collectingBehavior(remainingParts, newCollectedParts)
              }
          }
          // Send reverse tasks to slave actors
          slaveActors.zip(grouped).foreach { case (actor, part) =>
            actor ! SlaveProcess.reverse_string(part, context.self)
          }

          // Start collecting reversed parts
          collectingBehavior(grouped, Nil)
      }
    }
  }

  object SlaveProcess {
    sealed trait Command
    case class reverse_string(part: String, replyTo: ActorRef[MasterProcess.Command]) extends Command

    def apply(): Behavior[Command] = Behaviors.receiveMessage {
      case reverse_string(s, replyTo) =>
        replyTo ! MasterProcess.ReversedPart(s.reverse)
        Behaviors.stopped
    }
  }

  // Client per effettuare richieste
  object ClientProcess {
    sealed trait Command
    case class ReverseRequest(input: String) extends Command

    def apply(): Behavior[Command] = Behaviors.setup { context =>
      val masterActor = context.spawn(MasterProcess(), "master")

      Behaviors.receiveMessage {
        case ReverseRequest(input) =>
          val replyTo = context.messageAdapter[String] { reversed =>
            println(s"Reversed string: $reversed")
            ReverseRequest(input)
          }

          masterActor ! MasterProcess.long_reverse_string(input, replyTo)
          Behaviors.same
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val system = ActorSystem(ClientProcess(), "Es3")

    system.tell(ClientProcess.ReverseRequest("Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae, justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt. Cras dapibus. Vivamus elementum semper nisi. Aenean vulputate eleifend tellus. Aenean leo ligula, porttitor eu, consequat vitae, eleifend ac, enim. Aliquam lorem ante, dapibus in, viverra quis, feugiat a, tellus. Phasellus viverra nulla ut metus varius laoreet. Quisque rutrum. Aenean imperdiet. Etiam ultricies nisi vel augue. Curabitur ullamcorper ultricies nisi. Nam eget dui. Etiam rhoncus. Maecenas tempus, tellus eget condimentum rhoncus, sem quam semper libero, sit amet adipiscing sem neque sed ipsum. Nam qu"))
  
    Thread.sleep(5000)
    system.terminate()
  }
}
