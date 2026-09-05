package basc
package feedback

import cats.effect.IO

import io.circe.{ Codec, Json }
import org.http4s.{ Method, Request, Query, Uri }
import org.http4s.client.Client

import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.EffectCatsEffect.*
import japgolly.scalajs.react.vdom.html_<^.*


object Consul:

  case class ServiceWeights(
    Passing: Int,
    Warning: Int
  ) derives Codec.AsObject

  case class AgentService(
    ID: String,
    Service: String,
    Tags: Option[List[String]],
    Meta: Option[Map[String, String]],
    Port: Int,
    Address: String,
    TaggedAddresses: Option[Map[String, Json]],
    Weights: Option[ServiceWeights],
    EnableTagOverride: Boolean,
    Datacenter: Option[String]
  ) derives Codec.AsObject:
      private def feedbackUrl(path: String): Uri = Uri.unsafeFromString("http://" + Address + ":" + Port + "/feedback/" + path)
      lazy val stateUrl: Uri = Uri.unsafeFromString("http://" + Address + ":" + Port + "/state")
      def state(using httpClient: Client[IO]): IO[State] =
        import org.http4s.circe.CirceEntityDecoder.*
        httpClient.expect[State](stateUrl)
      def parameters(params: Parameters)(using httpClient: Client[IO]): IO[State] =
        import org.http4s.circe.CirceEntityEncoder.*
        val state = State(params)
        httpClient.successful(Request[IO](Method.PUT, stateUrl).withEntity(state)) >> this.state
      def exit(using httpClient: Client[IO]): IO[Boolean] =
        httpClient.expect[String](feedbackUrl("exit")).map(_.toBoolean)
      def exit(flag: Boolean)(using httpClient: Client[IO]): IO[Boolean] =
        httpClient.successful(Request[IO](Method.PUT, feedbackUrl("exit" + "/" + flag))) >> exit
      def pause(using httpClient: Client[IO]): IO[Boolean] =
        httpClient.expect[String](feedbackUrl("pause")).map(_.toBoolean)
      def pause(flag: Boolean)(using httpClient: Client[IO]): IO[Boolean] =
        httpClient.successful(Request[IO](Method.PUT, feedbackUrl("pause" + "/" + flag))) >> pause
      def stop(using httpClient: Client[IO]): IO[Boolean] =
        httpClient.expect[String](feedbackUrl("stop")).map(_.toBoolean)
      def stop(flag: Boolean)(using httpClient: Client[IO]): IO[Boolean] =
        httpClient.successful(Request[IO](Method.PUT, feedbackUrl("stop" + "/" + flag))) >> stop
      def traces(using httpClient: Client[IO]): IO[Boolean] =
        httpClient.expect[String](feedbackUrl("traces")).map(_.toBoolean)
      def traces(flag: Boolean)(using httpClient: Client[IO]): IO[Boolean] =
        httpClient.successful(Request[IO](Method.PUT, feedbackUrl("traces" + "/" + flag))) >> traces

  val defaultUrl = "http://localhost:8500"

  def apply(url: String, calculus: filter.Calculi, emitter: String, traces: filter.Traces): Option[String] =
    val service = if traces.service == filter.Traces.same then traces.toString else traces.service.toString
    val meta = List("calculus", "emitter", "backend", "producer").map("Meta." + _) zip List(calculus.tag.toString, emitter, traces.toString, service)
    val query = Query.empty.++?("filter", meta.map(_ + "==" + _))
    Uri.fromString(url.stripSuffix("/") + "/v1/agent/services").toOption.map(_.copy(query = query).toString)

  val Component = ScalaFnComponent[(Input, String => IO[Unit])] { (input, cb) =>

    def onConsulURLChange(e: ReactEventFromInput): IO[Unit] = cb(e.target.value)

    <.div(
      <.label(^.htmlFor := "consulurl-text", "Consul URL: "),

      <.input(
        ^.id            := "consulurl-text",
        ^.`type`        := "text",
        ^.placeholder   := defaultUrl,
        ^.value         := input.consulUrl,
        ^.size          := input.consulUrl.length max defaultUrl.length,
        ^.onChange      ==> onConsulURLChange
      )
    )
  }
