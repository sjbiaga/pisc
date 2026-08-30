/*
 * Copyright (c) 2023-2026 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * [Except as contained in this notice, the name of Sebastian I. Gliţa-Catina
 * shall not be used in advertising or otherwise to promote the sale, use
 * or other dealings in this Software without prior written authorization
 * from Sebastian I. Gliţa-Catina.]
 */

import _root_.cats.syntax.functor.*
import _root_.cats.effect.{ IO, Ref, Resource }
import _root_.com.comcast.ip4s.{ host, port, IpAddress, Hostname }
import _root_.io.circe.Codec
import _root_.org.http4s.circe.CirceEntityCodec.*
import _root_.org.http4s.dsl.Http4sDsl
import _root_.org.http4s.HttpRoutes
import _root_.org.http4s.server.{ Router, Server }
import _root_.org.http4s.ember.client.EmberClientBuilder
import _root_.org.http4s.ember.server.EmberServerBuilder


package object `Π-http4s`:

  import `Π-loop`.{ Feedback, `Π-Parameters` }
  import `Π-traces`.*
  import Traces.*


  enum Traces derives Codec.AsObject:
    case ConsoleCSV
    case FileCSV(filename: String)
    case AmazonSQS(backend: String, queue: String)
    case Kafka(backend: String, topic: String)
    case RabbitMQ(queue: String)

  object Traces:
    def apply(): Option[Traces] =
      Option(`π-traces`).map {
        case `Π-ConsoleCSV` => ConsoleCSV
        case `Π-FileCSV`(filename) => FileCSV(filename)
        case it @ `Π-AmazonSQS`(_, _, _, _, _, queue) => AmazonSQS(it.backend.toString, queue)
        case it @ `Π-Kafka`(_, _, _, topic: String) => Kafka(it.backend.toString, topic)
        case `Π-RabbitMQ`(_, _, queue) => RabbitMQ(queue)
      }


  case class Parameters(parallelism: Option[Int],
                        threshold: Option[Int],
                        timeout: Option[Int],
                        exit: Option[Boolean],
                        snapshot: Option[Boolean]) derives Codec.AsObject:
    def apply(default: `Π-Parameters`): `Π-Parameters` =
      `Π-Parameters`(default.address,
                     parallelism.getOrElse(default.parallelism),
                     threshold.getOrElse(default.threshold),
                     timeout.getOrElse(default.timeout),
                     exit.getOrElse(default.exit),
                     snapshot.getOrElse(default.snapshot))

  object Parameters:
    def apply(parameters: `Π-Parameters`): Parameters =
      Parameters(Some(parameters.parallelism),
                 Some(parameters.threshold),
                 Some(parameters.timeout),
                 Some(parameters.exit),
                 Some(parameters.snapshot))


  case class State(parameters: Parameters,
                   traces: Option[Traces],
                   last: Option[Long],
                   clock: Option[Double],
                   idle: Option[Long],
                   started: Option[Long],
                   done: Option[Boolean]) derives Codec.AsObject


  object BooleanVar:
    def unapply(self: String): Option[Boolean] =
      self.toLowerCase match
        case "0" | "off" | "false" | "no" | "n" => Some(false)
        case "1" | "on" | "true" | "yes" | "y"  => Some(true)
        case _                                  => None

  object FeedbackEndpoint extends Http4sDsl[IO]:
    def apply(feedback: Feedback) = HttpRoutes.of[IO] {
      case GET -> Root / "pause" =>
        feedback.pauseRD_stopR_exitRD.get.flatMap(_._1._1.tryGet).map(_ eq None).map(_.toString).flatMap(Ok(_))

      case GET -> Root / "traces" =>
        feedback.tracesR.get.map(_.toString).flatMap(Ok(_))

      case GET -> Root / "stop" =>
        feedback.pauseRD_stopR_exitRD.get.map(_._1._2.toString).flatMap(Ok(_))

      case GET -> Root / "exit" =>
        feedback.pauseRD_stopR_exitRD.get.flatMap(_._2.tryGet).map(_ ne None).map(_.toString).flatMap(Ok(_))

      case PUT -> Root / "pause" / BooleanVar(it) =>
        feedback.pauseRD_stopR_exitRD
          .evalModify { case ((pauseD, stop), exitD) =>
            if stop
            then
              (if it then Conflict() else Ok()).map(pauseD -> stop -> exitD -> _)
            else if it
            then
              pauseD.tryGet.map(_ eq None)
                .flatMap {
                  if _
                  then
                    Ok().map(pauseD -> stop -> exitD -> _)
                  else
                    (IO.deferred[Unit] product Ok()).map(_ -> stop -> exitD -> _)
                }
            else
              (pauseD.complete(()) >> Ok()).map(pauseD -> stop -> exitD -> _)
          }

      case PUT -> Root / "traces" / BooleanVar(it) =>
        feedback.tracesR.set(it) >> Ok()

      case PUT -> Root / "stop" / BooleanVar(it) =>
        feedback.pauseRD_stopR_exitRD
          .evalModify { case ((pauseD, stop), exitD) =>
            if stop
            then
              (if it then Ok() else Conflict()).map(pauseD -> stop -> exitD -> _)
            else
              (pauseD.complete(()) >> Ok()).map(pauseD -> it -> exitD -> _)
          }

      case PUT -> Root / "exit" / BooleanVar(it) =>
        feedback.pauseRD_stopR_exitRD
          .evalModify { case ((pauseD, stop), exitD) =>
            if it
            then
              (exitD.complete(()) >> Ok()).map(pauseD -> stop -> exitD -> _)
            else if stop
            then
              Conflict().map(pauseD -> stop -> exitD -> _)
            else
              exitD.tryGet.map(_ eq None)
                .flatMap {
                  if _
                  then
                    Ok().map(pauseD -> stop -> exitD -> _)
                  else
                    (IO.deferred[Unit] product Ok()).map(pauseD -> stop -> _ -> _)
                }
          }
    }


  object StateEndpoint extends Http4sDsl[IO]:

    def apply(batch: Boolean, startedR: Ref[IO, Long], feedback: Feedback) = HttpRoutes.of[IO] {
      case GET -> Root =>
        for
          started  <- startedR.get
          params   <- feedback.paramsR.get
          (last,
           clock)  <- feedback.lastR.get
          idle     <- IO.monotonic.map(_.toNanos - last)
          done     <- feedback.doneR.get
          state     = State(Parameters(params), Traces(), Some(last), Some(clock), Some(idle), Some(started), Some(done))
          response <- Ok(state)
        yield
          response

      case request @ PUT -> Root =>
        request.decode[State] {
          case State(_, Some(_), _, _, _, _, _)    =>
            BadRequest("attempt to alter the `traces' read-only value")
          case State(_, _, Some(_), _, _, _, _)    =>
            BadRequest("attempt to alter the `last' read-only value")
          case State(_, _, _, Some(_), _, _, _)    =>
            BadRequest("attempt to alter the `clock' read-only value")
          case State(_, _, _, _, Some(_), _, _)    =>
            BadRequest("attempt to alter the `idle' read-only value")
          case State(_, _, _, _, _, Some(_), _)    =>
            BadRequest("attempt to alter the `started' read-only counter")
          case State(_, _, _, _, _, _, Some(_))    =>
            BadRequest("attempt to alter the `done' read-only flag")
          case State(Parameters(_,Some(threshold), _, _, _), _, _, _, _, _, _) if ((0 max threshold) > 0) != batch =>
            BadRequest(s"attempt to change the ${if batch then "" else "non-"}batch mode through the `threshold' parameter")
          case State(parameters, _, _, _, _, _, _) =>
            feedback.paramsR.get.flatMap { default =>
              var params = parameters(default)
              params = params.copy(parallelism = 1 max params.parallelism,
                                   threshold = 0 max params.threshold,
                                   timeout = 0 max params.timeout)
              feedback.paramsRD.get.flatMap(_.complete(params)) >> Ok()
            }
        }
    }


  object HealthCheckEndpoint extends Http4sDsl[IO]:

    def apply() = HttpRoutes.of[IO] {
      case GET -> Root => Ok("OK")
    }


  def http4s(address: String, batch: Boolean, startedR: Ref[IO, Long], feedback: Feedback): Resource[IO, Server] =
    val baApp = Router[IO](
      "feedback" -> FeedbackEndpoint(feedback),
      "state" -> StateEndpoint(batch, startedR, feedback),
      "health" -> HealthCheckEndpoint()
    ).orNotFound
    EmberServerBuilder
      .default[IO]
      .withHost(IpAddress.fromString(address).orElse(Hostname.fromString(address)).getOrElse(host"localhost"))
      .withPort(port"0")
      .withHttpApp(baApp)
      .build

  case class ConsulCheck(HTTP: String, Interval: String, Timeout: String) derives Codec.AsObject
  case class ConsulRegister(ID: String, Name: String, Address: String, Port: Int, Tags: List[String], Meta: Map[String, String], Check: ConsulCheck) derives Codec.AsObject

  val serviceName = "BioAmbients2Scala"

  def http4s(batch: Boolean, server: Server): Resource[IO, Unit] =
    import _root_.org.http4s.Method.PUT
    import _root_.org.http4s.{ Request, Uri }

    Option {
      Traces().fold(null) {
        case AmazonSQS(backend, queue) => ("amazonsqs", backend, "queue", queue)
        case Kafka(backend, topic) => ("kafka", backend, "topic", topic)
        case RabbitMQ(queue) => ("rabbitmq", "rabbitmq", "queue", queue)
        case _ => null
      }
    } match
      case Some((producer, backend, kind, name)) =>
        val host = server.address.getAddress.getHostAddress
        val port = server.address.getPort
        val consulAddr = sys.env.get("CONSUL_HTTP_ADDR").getOrElse(s"$host:8500")
        val consulBase = Uri.unsafeFromString(s"http://$consulAddr/v1/agent")
        val serviceId = s"$serviceName-$name-$port"
        val registrationPayload = ConsulRegister(
          ID = serviceId,
          Name = serviceName,
          Address = host,
          Port = port,
          Tags = List(producer, name),
          Meta = Map(
            "batch" -> batch.toString,
            "producer" -> producer,
            "backend" -> backend,
            "kind" -> kind,
            "emitter" -> "cef",
            "pid" -> ProcessHandle.current.pid.toString
          ),
          Check = ConsulCheck(
            HTTP = s"http://$host:$port/health",
            Interval = "10s",
            Timeout = "2s"
          )
        )
        EmberClientBuilder.default[IO].build.flatMap { client =>
          Resource.make {
            client.successful(Request[IO](PUT, consulBase / "service" / "register").withEntity(registrationPayload)).flatTap {
              if _
              then IO.println(s"✅ Successfully registered '$serviceId' to Consul on port $port.")
              else IO.println(s"⚠ Failed to register '$serviceId' to Consul on port $port.")
            }.handleErrorWith(err => IO.println(s"🛑 Error during Consul setup (on port $port): ${err.getMessage}").as(false))
          } {
            if _
            then
              client.successful(Request[IO](PUT, consulBase / "service" / "deregister" / serviceId)).flatMap {
                if _
                then IO.println(s"🛑 Successfully deregistered '$serviceId' from Consul.")
                else IO.println(s"⚠ Failed to cleanly deregister '$serviceId' from Consul.")
              }.handleErrorWith(err => IO.println(s"❌ Error during Consul cleanup: ${err.getMessage}"))
            else
              IO.unit
          }.void
        }
      case _ =>
        Resource.unit
