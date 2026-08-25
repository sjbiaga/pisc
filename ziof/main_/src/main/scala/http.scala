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

import zio.*
import zio.http.*
import zio.schema.*
import zio.schema.codec.JsonCodec.schemaBasedBinaryCodec


package object `Π-http`:

  import `Π-loop`.{ Feedback, `Π-Parameters` }
  import `Π-traces`.*
  import Traces.*


  enum Traces:
    case ConsoleCSV
    case FileCSV(filename: String)
    case AmazonSQS(queue: String)
    case Kafka(topic: String)
    case RabbitMQ(queue: String)

  object Traces:
    def apply(): Option[Traces] =
      Option(`π-traces`).map {
        case `Π-ConsoleCSV` => ConsoleCSV
        case `Π-FileCSV`(filename) => FileCSV(filename)
        case `Π-AmazonSQS`(_, _, _, _, queue) => AmazonSQS(queue)
        case `Π-Kafka`(_, topic: String) => Kafka(topic)
        case `Π-RabbitMQ`(_, _, queue) => RabbitMQ(queue)
      }


  case class Parameters(address: Option[String],
                        parallelism: Option[Int],
                        threshold: Option[Int],
                        timeout: Option[Int],
                        exit: Option[Boolean]):
    def apply(default: `Π-Parameters`): `Π-Parameters` =
      `Π-Parameters`(address.getOrElse(default.address),
                     parallelism.getOrElse(default.parallelism),
                     threshold.getOrElse(default.threshold),
                     timeout.getOrElse(default.timeout),
                     exit.getOrElse(default.exit))

  object Parameters:
    def apply(parameters: `Π-Parameters`): Parameters =
      Parameters(Some(parameters.address),
                 Some(parameters.parallelism),
                 Some(parameters.threshold),
                 Some(parameters.timeout),
                 Some(parameters.exit))


  case class State(parameters: Parameters,
                   traces: Option[Traces],
                   last: Option[Long],
                   clock: Option[Double],
                   idle: Option[Long],
                   started: Option[Long],
                   done: Option[Boolean])

  object State:

    given Schema[State] = DeriveSchema.gen[State]


  object FeedbackRoutes:

    def apply(feedback: Feedback) = Routes(
      Method.GET / "feedback" / "pause" -> handler(
        feedback.pauseRP.get.flatMap(_.isDone.negate).map(_.toString).map(Response.text)
      ),
      Method.GET / "feedback" / "traces" -> handler(
        feedback.tracesR.get.map(_.toString).map(Response.text)
      ),
      Method.GET / "feedback" / "stop" -> handler(
        feedback.stopR.get.map(_.toString).map(Response.text)
      ),
      Method.GET / "feedback" / "exit" -> handler(
        feedback.exitRP.get.flatMap(_.isDone).map(_.toString).map(Response.text)
      ),
      Method.PUT / "feedback" / "pause" / boolean("it") -> handler { (it: Boolean, _: Request) =>
        if it
        then
          feedback.pauseRP.get.flatMap(_.isDone.negate).flatMap {
            if _
            then
              ZIO.succeed(Response.ok)
            else
              Promise.make[Nothing, Unit].flatMap(feedback.pauseRP.set).as(Response.ok)
          }
        else
          feedback.pauseRP.get.flatMap(_.succeed(())).as(Response.ok)
      },
      Method.PUT / "feedback" / "traces" / boolean("it") -> handler { (it: Boolean, _: Request) =>
        feedback.tracesR.set(it).as(Response.ok)
      },
      Method.PUT / "feedback" / "stop" / boolean("it") -> handler { (it: Boolean, _: Request) =>
        feedback.stopR.get.flatMap {
          if _
          then
            ZIO.succeed(Response.ok)
          else
            feedback.stopR.set(it).as(Response.ok)
        }
      },
      Method.PUT / "feedback" / "exit" / boolean("it") -> handler { (it: Boolean, _: Request) =>
        if it
        then
          feedback.exitRP.get.flatMap(_.succeed(())).as(Response.ok)
        else
          feedback.exitRP.get.flatMap(_.isDone).flatMap {
            if _
            then
              Promise.make[Nothing, Unit].flatMap(feedback.exitRP.set).as(Response.ok)
            else
              ZIO.succeed(Response.ok)
          }
      }
    )


  object StateRoutes:

    def apply(batch: Boolean, startedR: Ref[Long], feedback: Feedback) = Routes(
      Method.GET / "state" -> handler { (_: Request) =>
        for
          started       <- startedR.get
          params        <- feedback.paramsR.get
          (last, clock) <- feedback.lastR.get
          idle          <- Clock.nanoTime.map(_ - last)
          done          <- feedback.doneR.get
          state          = State(Parameters(params), Traces(), Some(last), Some(clock), Some(idle), Some(started), Some(done))
        yield
          Response.ok.copy(body = Body.from(state))
      },
      Method.PUT / "state" -> handler { (request: Request) =>
        request.body.to[State].either.flatMap {
          case Left(err) =>
            ZIO.succeed(Response.badRequest)
          case Right(state) =>
            state match
              case State(_, Some(_), _, _, _, _, _)    =>
                ZIO.succeed(Response.badRequest("attempt to alter the `traces' read-only value"))
              case State(_, _, Some(_), _, _, _, _)    =>
                ZIO.succeed(Response.badRequest("attempt to alter the `last' read-only value"))
              case State(_, _, _, Some(_), _, _, _)    =>
                ZIO.succeed(Response.badRequest("attempt to alter the `clock' read-only value"))
              case State(_, _, _, _, Some(_), _, _)    =>
                ZIO.succeed(Response.badRequest("attempt to alter the `idle' read-only value"))
              case State(_, _, _, _, _, Some(_), _)    =>
                ZIO.succeed(Response.badRequest("attempt to alter the `started' read-only counter"))
              case State(_, _, _, _, _, _, Some(_))    =>
                ZIO.succeed(Response.badRequest("attempt to alter the `done' read-only flag"))
              case State(Parameters(Some(_), _, _, _, _), _, _, _, _, _, _) =>
                ZIO.succeed(Response.badRequest("attempt to change the `address' read-only parameter"))
              case State(Parameters(_, _, Some(threshold), _, _), _, _, _, _, _, _) if ((0 max threshold) > 0) != batch =>
                ZIO.succeed(Response.badRequest(s"attempt to change the ${if batch then "" else "non-"}batch mode through the `threshold' value"))
              case State(parameters, _, _, _, _, _, _) =>
                feedback.paramsR.get.flatMap { default =>
                  var params = parameters(default)
                  params = params.copy(parallelism = 1 max params.parallelism,
                                       threshold = 0 max params.threshold,
                                       timeout = 0 max params.timeout)
                  feedback.paramsRP.get.flatMap(_.succeed(params))
                }.as(Response.ok)
        }
      }
    )


  object HealthCheckRoutes:

    def apply() = Routes(
      Method.GET / "health" -> handler(Response.text("OK"))
    )


  case class ConsulCheck(HTTP: String, Interval: String, Timeout: String)
  case class ConsulRegister(ID: String, Name: String, Address: String, Port: Int, Tags: List[String], Check: ConsulCheck)
  object ConsulRegister:
    given Schema[ConsulRegister] = DeriveSchema.gen[ConsulRegister]

  val serviceName = "StochasticPiCalculus2Scala"

  def http(address: String): ZLayer[Any, Throwable, Server.Config] =
    ZLayer.succeed(Server.Config.default.binding(address, 0))

  def http(address: String, batch: Boolean, started: Ref[Long], feedback: Feedback)(body: UIO[ExitCode]): URIO[Client & Server, ExitCode] =
    Option {
      Traces().fold(null) {
        case AmazonSQS(queue) => "amazonsqs" -> queue
        case Kafka(topic) => "kafka" -> topic
        case RabbitMQ(queue) => "rabbitmq" -> queue
        case _ => null
      }
    } match
      case Some((tag, name)) =>
        for
          port <- Server.install(FeedbackRoutes(feedback) ++ StateRoutes(batch, started, feedback) ++ HealthCheckRoutes())
          host  = address
          consulAddr = sys.env.get("CONSUL_HTTP_ADDR").getOrElse(s"$host:8500")
          consulBase = URL.decode(s"http://$consulAddr/v1/agent").right.get
          serviceId = s"$serviceName-$name-$port"
          registrationPayload = ConsulRegister(
            ID = serviceId,
            Name = serviceName,
            Address = host,
            Port = port,
            Tags = List(tag, "ziof_emitter"),
            Check = ConsulCheck(
              HTTP = s"http://$host:$port/health",
              Interval = "10s",
              Timeout = "2s"
            )
          )
          cons <- Client
                    .batched(Request.put(consulBase / "service" / "register", Body.from(registrationPayload)))
                    .either
                    .flatMap {
                      case Left(err)   =>
                        ZIO.debug(s"🛑 Error during Consul setup (on port $port): ${err.getMessage}").as(false)
                      case Right(resp) =>
                        if resp.status.isError
                        then
                          ZIO.debug(s"⚠ Failed to register '$serviceId' to Consul on port $port.").as(false)
                        else
                          ZIO.debug(s"✅ Successfully registered '$serviceId' to Consul on port $port.").as(true)
                    }
          code <- body
          _    <- ( if cons
                    then
                      Client
                        .batched(Request.put(consulBase / "service" / "deregister" / serviceId, Body.empty))
                        .either
                        .flatMap {
                          case Left(err)   =>
                            ZIO.debug(s"❌ Error during Consul cleanup: ${err.getMessage}")
                          case Right(resp) =>
                            if resp.status.isError
                            then
                              ZIO.debug(s"⚠ Failed to cleanly deregister '$serviceId' from Consul.")
                            else
                              ZIO.debug(s"🛑 Successfully deregistered '$serviceId' from Consul.")
                        }
                    else
                      ZIO.unit
                  )
        yield
          code
      case _ =>
        body
