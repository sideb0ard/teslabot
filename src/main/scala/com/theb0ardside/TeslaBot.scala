package com.theb0ardside

import akka.actor.{ Actor, ActorLogging, ActorSystem, ActorRef, Props, Terminated }
import akka.io.{ IO, Tcp }
import akka.util.{ByteString, Timeout}
import akka.pattern.ask
import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit
import scala.io.StdIn
import scala.collection.immutable.StringOps
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

case class Message(msg: String)
case class Person(var name: String)
case object AskNameMessage

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
  implicit val timeout = Timeout(10 seconds)

  val p = Person("")
  connection ! Tcp.Write(ByteString.apply("I'M " + TeslaBot.name + ". WHO THE FUCK ARE YOU?\n"))

  def receive: Receive = {
    case Tcp.Received(data) =>
      val name = data.utf8String.trim
      log.debug("Received {} from remote address {}", name, remote)
      name match {
        case "close" => context.stop(self)
        case _ => { 
          println("NAME IS " + name)
          p.name = name
          sender ! Tcp.Write(ByteString.apply("PLEASED TO MEET YOU, " + p.name.toUpperCase + ". WHAT'D YA WANNA YAMMER ABOUT?\n"))
          context.become(converse)
        }
      }
    case _: Tcp.ConnectionClosed =>
      log.debug("Stopping, because remote address {} closed", remote)
      context.stop(self)
    case Terminated(`connection`) =>
      log.debug("Stopping, remote {} died", remote)
  }

  def converse: Receive = {
    case Tcp.Received(data) =>
      val text = data.utf8String.trim
      log.debug("Received {} from remote address {}", text, remote)
      text match {
        case "close" => context.stop(self)
        case _ => sender ! Tcp.Write(
          ByteString.apply(
              Await.result(lp ? Message(text), timeout.duration).asInstanceOf[String]))
      }
    case _: Tcp.ConnectionClosed =>
      log.debug("Stopping, because remote address {} closed", remote)
      context.stop(self)
    case Terminated(`connection`) =>
      log.debug("Stopping, remote {} died", remote)
  }
}

class LanguageProcessor extends Actor with ActorLogging {
  def receive: Receive = {
    case Message(msg) => 
      //println("YEH, BOT, GOT ME SOME " + msg)
      sender ! "FUCK YEH, " + msg.toUpperCase + "!!\n"
  }
}

