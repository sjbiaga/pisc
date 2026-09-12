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
import japgolly.scalajs.react.extra.useStateSnapshotWithReuse
import japgolly.scalajs.react.vdom.html_<^.*


case class Input(consulUrl: String = Consul.defaultUrl,
                 calculi: filter.Calculi.State = filter.Calculi.State(selectedCalculus = "bioambients"),
                 effects: filter.Effects.State = filter.Effects.State(selectedEffect = "cats.effect.IO"),
                 emitters: filter.Emitters.State = filter.Emitters.State(selectedEmitter = "ce"),
                 traces: filter.Traces.State = filter.Traces.State(selectedTraces = "elasticmq"))


@JSExportTopLevel("main")
object Main extends IOApp:

  val Component = ScalaFnComponent[Client[IO]] { implicit httpClient =>

    for
      input   <- useState(Input())
      output  <- useStateSnapshotWithReuse(Output())
      restore <- useStateSnapshotWithReuse(Restore())
      callback = IO.defer {
        val traces = filter.Traces.valueOf(input.value.traces.selectedTraces)
        Consul(
          input.value.consulUrl,
          filter.Calculi.valueOf(input.value.calculi.selectedCalculus),
          input.value.effects.selectedEffect,
          input.value.emitters.selectedEmitter,
          traces
        ) match
          case Some(url) =>
            for
              m <- httpClient.expect[Map[String, Consul.AgentService]](url)
              r <- SignallingRef[IO, Boolean](false)
              l <- m.toList.zipWithIndex.filter(_._1._2.Weights.get.Passing > 0).traverse { case ((key, service), i) =>
                     for
                       a <- service.state
                       x <- service.exit
                       z <- service.pause
                       s <- service.stop
                       t <- service.traces
                     yield
                       Item(key, service, i, r, a, x, z, s, t, traces)
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

        <.button(^.onClick --> callback, "Query"),
        <.div(Consul.Component(input.value -> { consulURL => input.modState(_.copy(consulUrl = consulURL)).to[IO] })),
        <.div(filter.Calculi.Component(input.value -> { calculus => input.modState(_.copy(calculi = input.value.calculi.copy(selectedCalculus = calculus))).to[IO] })),
        <.div(filter.Effects.Component(input.value -> { effect => input.modState(_.copy(effects = input.value.effects.copy(selectedEffect = effect))).to[IO] })),
        <.div(filter.Emitters.Component(input.value -> { emitter => input.modState(_.copy(emitters = input.value.emitters.copy(selectedEmitter = emitter))).to[IO] })),
        <.div(filter.Traces.Component(input.value -> { traces => input.modState(_.copy(traces = input.value.traces.copy(selectedTraces = traces))).to[IO] })),

        <.h2("Output"),

        <.div(Output.Component(Output.Props(output, restore))),

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
      val component = Component(httpClient)
      IO(ReactDOMClient.createRoot(container).render(component)).as(ExitCode.Success) <* IO.never
    }
