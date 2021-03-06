package com.theb0ardside

import akka.actor.{ Actor, ActorLogging, ActorSystem, ActorRef, Props, Terminated }
import akka.io.{ IO, Tcp }
import akka.util.{ByteString, Timeout}
import akka.pattern.ask
import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit
import scala.util.Random
import scala.io.StdIn
import scala.collection.immutable.StringOps
import scala.collection.mutable.Map
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

import org.json4s._
import org.json4s.jackson.JsonMethods._


case class Message(msg: String)
case class Person(var name: String)
case class Keywurd(keywurd: String, score: Int, decomp_rules: List[Map[String, List[String]]])

object TeslaBotMain extends App {
  val system = ActorSystem("teslabot-system")
  val endpoint = new InetSocketAddress("localhost", 8088)
  system.actorOf(TeslaBot.props(endpoint), "teslabot-service")
  StdIn.readLine(f"Hit ENTER to exit ...\n")
  system.shutdown()
}

object TeslaBot {
  val name = ":TESLABOT:"
  var peopleIKnow = List[Person]()

  implicit val formats = DefaultFormats

  val lnz = scala.io.Source.fromFile("src/main/resources/newlang.json").mkString
  val json = parse(lnz)
  val list_of_jkwz = (json \ "keywurdz").children
  val kwz = for (k <- list_of_jkwz) yield k.extract[Keywurd]

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

  //val steps = Map[Int, String]()
  //steps(0) =  "WHAT SHALL WE SPEAK OF?"
  //val steps = Map(0->"name",1->"WHAT SHALL WE SPEAK OF?")

  val p = Person("")
  connection ! Tcp.Write(ByteString.apply("\nI'M " + TeslaBot.name + " WHAT IS YOUR NAME?\n> "))

  def convrsrrr(n: Int): Receive = {
    case Tcp.Received(data) =>
      val txt = data.utf8String.trim
      log.debug("Received {} from remote address {}", txt, remote)
      txt match {
        case "close" => context.stop(self)
        case _ =>
          if ( n == 0 ) {
            p.name = txt
            sender ! Tcp.Write(ByteString.apply("\nPLEASED TO MEET YOU " + p.name.toUpperCase + ".\nWHA'S GON' ON?\n> "))
            context.become(convrsrrr(n+1))
          } else {
            // MEAT AND POTATOES RIGHT HERE - SEND MSG TXT TO BE TRANSFORMED THEN REPLY
            val reply = Await.result(lp ? Message(txt), timeout.duration).asInstanceOf[String]
            sender ! Tcp.Write(ByteString.apply("\n" + reply + "\n> "))
            context.become(convrsrrr(n+1))
          }
      }
    case _: Tcp.ConnectionClosed =>
      log.debug("Stopping, because remote address {} closed", remote)
      context.stop(self)
    case Terminated(`connection`) =>
      log.debug("Stopping, remote {} died", remote)
  }

  // FIRST MESSAGE, CALL CONVRSRRR WITH ZERO SO IT KNOWS TO SAY HELLO
  def receive = convrsrrr(0)

}

class LanguageProcessor extends Actor with ActorLogging {
  def hasKeyWurd(keyWurd: String, text: String): Boolean = (keyWurd.r findFirstIn text).nonEmpty
  def receive: Receive = {
    case Message(msg) => 
      //println("YEH, BOT, GOT ME SOME " + msg)
      //TeslaBot.keywurds.foreach(p => println(">>> key=" + p._1 + ", val:" + p._2))
      var reply = "boringDefaultReply"
      var reassmb = new String
      var goTo = new String
      var rank = -2
      // this next bit is all copied from perl's Chatbot::Eliza ->
      // convert punctuation to periods, then separate into sentences
      // to look for keywurds. Compare score of keyword with any previous, 
      // if this is higher, set 
      var msgClean = """\?""".r replaceAllIn (msg, ".")
      msgClean = """\!""".r replaceAllIn (msgClean, ".")
      msgClean = """\,""".r replaceAllIn (msgClean, ".")
      msgClean = """but""".r replaceAllIn (msgClean, ".")
      println("MSG NOW" + msgClean)
      var msgPartz = msgClean.split("""\.""")
      //println(msgPartz)
      msgPartz.foreach {
        case mp =>
          println( mp + "\n" )

          for (k <- TeslaBot.kwz) {
            //println(k.keywurd) 
            if (hasKeyWurd(k.keywurd, mp)) {
              println("JOBBITESSS!")
            }
          }
      }
      //println("RANK IS:", rank)
      sender ! reply.toUpperCase
  }
}

