package basc
package feedback

import cats.instances.list.*
import cats.syntax.traverse.*

import cats.effect.{ ExitCode, IO, IOApp }

import fs2.concurrent.SignallingRef

import scala.scalajs.js.annotation.JSExportTopLevel

import org.http4s.client.Client
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.dom.FetchClientBuilder

import org.scalajs.dom.document

import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.useStateSnapshot
import japgolly.scalajs.react.util.EffectCatsEffect.*
import japgolly.scalajs.react.vdom.html_<^.*


case class Input(consulUrl: String = Consul.defaultUrl,
                 calculi: filter.Calculi.State = filter.Calculi.State(selectedCalculus = "stochasticpicalculus"),
                 emitters: filter.Emitters.State = filter.Emitters.State(selectedEmitter = "ce"),
                 traces: filter.Traces.State = filter.Traces.State(selectedTraces = "amazonsqs"))


@JSExportTopLevel("main")
object Main extends IOApp:

  def Component(using Client[IO]) = ScalaFnComponent[Unit] { _ =>
    for
      input   <- useState(Input())
      output  <- useStateSnapshot(Output())
      restore <- useStateSnapshot(Restore())
      callback = IO.defer {
        Consul(input.value.consulUrl, filter.Calculi.valueOf(input.value.calculi.selectedCalculus), input.value.emitters.selectedEmitter, filter.Traces.valueOf(input.value.traces.selectedTraces)) match
          case Some(url) =>
            for
              m <- summon[Client[IO]].expect[Map[String, Consul.AgentService]](url)
              r <- SignallingRef[IO, Boolean](false)
              l <- m.toList.zipWithIndex.filter(_._1._2.Weights.get.Passing > 0).traverse { case ((key, service), i) =>
                     for
                       a <- service.state
                       x <- service.exit
                       z <- service.pause
                       s <- service.stop
                       t <- service.traces
                     yield
                       Item(key, service, i, r, a, x, z, s, t)
                   }
              _ <- output.setState(Output(l)).to[IO]
              _ <- restore.setState(Restore(l.zipWithIndex.map(_.state.parameters -> _))).to[IO]
            yield
              ()
          case _ =>
            IO.unit
      }.handleErrorWith(t => IO { document.title = s"Oops: ${t.getMessage}" })
    yield
      <.div(
        <.h2("Input"),

        <.button(
          ^.onClick --> callback,
          "Query"
        ),
        <.div(Consul.Component(input.value -> { consulURL => input.modState(_.copy(consulUrl = consulURL)).to[IO] })),
        <.div(filter.Calculi.Component(input.value -> { calculus => input.modState(_.copy(calculi = input.value.calculi.copy(selectedCalculus = calculus))).to[IO] })),
        <.div(filter.Emitters.Component(input.value -> { emitter => input.modState(_.copy(emitters = input.value.emitters.copy(selectedEmitter = emitter))).to[IO] })),
        <.div(filter.Traces.Component(input.value -> { traces => input.modState(_.copy(traces = input.value.traces.copy(selectedTraces = traces))).to[IO] })),

        <.h2("Output"),

        <.div(Output.Component(output, restore)),

        <.footer(<.p("© 2026 Sebastian I. Gliţa-Catina"))
      )
  }

  override def run(args: List[String]): IO[ExitCode] =
    ( for
        httpClient <- FetchClientBuilder[IO].resource
      yield
        httpClient
    ).use { httpClient =>
      val container = document.getElementById("root")
      val component = Component(using httpClient)()
      IO(ReactDOMClient.createRoot(container).render(component)).as(ExitCode.Success) <* IO.never
    }
