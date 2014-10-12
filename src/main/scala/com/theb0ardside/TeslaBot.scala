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
    Map("xnone"-> (-1, Map("jobbone" -> List("innerjoabie", "innerjoabie2"), "jobbtwp" -> List("inn2job", "iun2j"))),
      "sorry"  -> ( 0, Map("sozz" -> List("innersozzee", "innerssssie2"), "sobbtwp" -> List("ssinn2job", "sziun2j"))),
      "jobito" -> (10, Map("JIAIAIA" -> List("zzz", "dfdfdf"), "jozz" -> List("JOBB", "JOJJJJinnerssssie2"), "jRRRRwp" -> List("JJJJJJjob", "JJJJn2j")))
    )

  val lnz = scala.io.Source.fromFile("src/main/resources/language.json").mkString
  val keywurdz_json = parse(lnz)

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

          val repliz = for {
            //JString(keywurd) <- TeslaBot.keywurdz_json \\ "keywurd"
            JObject(k) <- TeslaBot.keywurdz_json
            JField("keywurd", JString(keywurd)) <- k
            //if keywurd contains JString("not")
            //JField("decomp", JArray(decomp)) <- k
          //} yield decomp
          } yield keywurd

          repliz.foreach {
            case k =>
              if (hasKeyWurd(k, mp)) {
                println("K:" + k)
              }
          }
          //println( kwz + "\n" )
            //TeslaBot.kwz.foreach {
            //  case (k) =>
            //    if (hasKeyWurd(k, mp)) { 
            //      println("OHYA: ", k ) 
            //    }
            //}
            //TeslaBot.keywurds.foreach {
            //  case (k,v) =>
            //    if (hasKeyWurd(k, mp) && v._1 > rank) {
            //      val rng = 0 to (v._2.size - 1)
            //      println("FOUND!: ", k, " IN ", mp, "\n\n")
            //      //println("ONE SLICE -:", v)
            //      println("TWO SLICE -:", v._2.keys)
            //      // TRY TO MATCH A DECOMPOSITION RULE HERE
            //      rank = v._1
            //    };
            //}

            // POSSIBILITIES

            //println("POSS: ", newReply_poss)
      }
      //println("RANK IS:", rank)
      sender ! reply.toUpperCase
  }
}

