package com.theb0ardside

import akka.actor.{ Actor, ActorLogging, ActorSystem, ActorRef, Props, Terminated }
import akka.io.{ IO, Tcp }
import akka.util.{ByteString, Timeout}
import akka.pattern.ask
import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit
import scala.io.StdIn
import scala.collection.immutable.StringOps
import scala.collection.mutable.Map
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

case class Message(msg: String)
case class Person(var name: String)

object TeslaBotMain extends App {
  val system = ActorSystem("teslabot-system")
  val endpoint = new InetSocketAddress("localhost", 8088)
  system.actorOf(TeslaBot.props(endpoint), "teslabot-service")
  StdIn.readLine(f"Hit ENTER to exit ...\n")
  system.shutdown()
}

object TeslaBot {
  val name = "::TESLA::BOT::"
  var peopleIKnow = List[Person]()
  def props(endpoint: InetSocketAddress): Props =
    Props(new TeslaBot(endpoint))
}

class TeslaBot(endpoint: InetSocketAddress) extends Actor with ActorLogging {
  import context.system
  IO(Tcp) ! Tcp.Bind(self, endpoint)

  override def receive: Receive = {
    case Tcp.Connected(remote, _) =>
      log.debug("Remote Address {} connected", remote)
      sender ! Tcp.Register(context.actorOf(TeslaBotConnectionHandler.props(remote, sender)))
  }
}

object TeslaBotConnectionHandler {
  def props(remote: InetSocketAddress, connection: ActorRef): Props =
    Props(new TeslaBotConnectionHandler(remote, connection))
}

class TeslaBotConnectionHandler(remote: InetSocketAddress, connection: ActorRef) extends Actor with ActorLogging {
  context.watch(connection)

  val lp = context.actorOf(Props[LanguageProcessor])
  implicit val timeout = Timeout(1 seconds)

  val steps = Map[Int, String]()
  steps(0) =  "WHAT SHALL WE SPEAK OF?"

  val p = Person("")
  connection ! Tcp.Write(ByteString.apply("I'M " + TeslaBot.name + ". WHAT IS YOUR NAME\n"))

  def convrsrrr(n: Int): Receive = {
    case Tcp.Received(data) =>
      val txt = data.utf8String.trim
      log.debug("Received {} from remote address {}", txt, remote)
      txt match {
        case "close" => context.stop(self)
        case _ =>
          if ( n == 0 ) {
            p.name = txt
            sender ! Tcp.Write(ByteString.apply("PLEASED TO MEET YOU " + p.name + ".\n"))
            context.become(convrsrrr(n+1))
          }
          steps(n+1) =
            Await.result(lp ? Message(txt), timeout.duration).asInstanceOf[String]
          sender ! Tcp.Write(ByteString.apply("ITERATION:" + n + " - " + steps(n) + "\n"))
          context.become(convrsrrr(n+1))
      }
    case _: Tcp.ConnectionClosed =>
      log.debug("Stopping, because remote address {} closed", remote)
      context.stop(self)
    case Terminated(`connection`) =>
      log.debug("Stopping, remote {} died", remote)
  }

  def receive = convrsrrr(0)

}

class LanguageProcessor extends Actor with ActorLogging {
  def receive: Receive = {
    case Message(msg) => 
      //println("YEH, BOT, GOT ME SOME " + msg)
      sender ! "FUCK YEH, " + msg.toUpperCase + "!!\n"
  }
}

