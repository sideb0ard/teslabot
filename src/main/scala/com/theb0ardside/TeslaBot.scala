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

object TeslaBotServiceApp extends App {
  val system = ActorSystem("teslabot-system")
  val endpoint = new InetSocketAddress("localhost", 8088)
  system.actorOf(TeslaBotService.props(endpoint), "teslabot-service")
  StdIn.readLine(f"Hit ENTER to exit ...\n")
  system.shutdown()
}

object TeslaBotService {
  def props(endpoint: InetSocketAddress): Props = 
    Props(new TeslaBotService(endpoint))
}

class TeslaBotService(endpoint: InetSocketAddress) extends Actor with ActorLogging {
  import context.system
  IO(Tcp) ! Tcp.Bind(self, endpoint)

  val name = "::TESLA::BOT::"

  override def receive: Receive = {
    case Tcp.Connected(remote, _) =>
      log.debug("Remote Address {} connected", remote)
      sender ! Tcp.Register(context.actorOf(TeslaBotConnectionHandler.props(remote, sender, name)))
      sender ! Tcp.Write(ByteString.apply("I'M " + name + ". WHO THE FUCK ARE YOU?\n"))
  }
}

object TeslaBotConnectionHandler {
  def props(remote: InetSocketAddress, connection: ActorRef, name: String): Props =
    Props(new TeslaBotConnectionHandler(remote, connection, name))
}

class TeslaBotConnectionHandler(remote: InetSocketAddress, connection: ActorRef, name: String) extends Actor with ActorLogging {
  context.watch(connection)

  val lp = context.actorOf(Props[LanguageProcessor])
  implicit val timeout = Timeout(1 second)

  def receive: Receive = {
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

