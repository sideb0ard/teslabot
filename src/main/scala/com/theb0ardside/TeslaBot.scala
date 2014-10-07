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
  val name = ":TESLABOT:"
  var peopleIKnow = List[Person]()
  val keywurds =
    Map("xnone"-> (-1, List(Map("jobbone" -> List("innerjoabie", "innerjoabie2")), Map("jobbtwp" -> List("inn2job", "iun2j")))),
      "sorry"-> (0, List(Map("sozz" -> List("innersozzee", "innerssssie2")), Map("sobbtwp" -> List("ssinn2job", "sziun2j")))),
"jobito"-> (10, List(Map("JIAIAIA" -> List("zzz", "dfdfdf")), Map("jozz" -> List("JOBB", "JOJJJJinnerssssie2")), Map("jRRRRwp" -> List("JJJJJJjob", "JJJJn2j"))))
    )
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
            TeslaBot.keywurds.foreach {
              case (k,v) =>
                if (hasKeyWurd(k, mp) && v._1 > rank) {
                  val rng = 0 to (v._2.size - 1)
                  println("FOUND!: ", k, " IN ", mp) 
                  println("BLSH - RANK:", v._2.slice(1,2), rng) //, Random.Shuffle(v._2).head)
                  println("RANDY :", rng(Random.nextInt(rng length)))
                  //reply = Random.shuffle(v._2.toList).head
                  //reply = Random.shuffle(v._2).head
                  //reply = v._2
                  rank = v._1
                };
            }
      }
      println("RANK IS:", rank)
      sender ! reply.toUpperCase
  }
}

