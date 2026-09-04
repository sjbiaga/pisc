package basc
package feedback

import cats.effect.IO

import fs2.concurrent.SignallingRef

import io.circe.Codec
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client

import monocle.Focus

import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.ReactMonocle.*
import japgolly.scalajs.react.util.EffectCatsEffect.*
import japgolly.scalajs.react.vdom.html_<^.*

enum Traces derives Codec.AsObject:
  case AmazonSQS(backend: String, queue: String)
  case Kafka(backend: String, topic: String)
  case RabbitMQ(queue: String)

case class Parameters(parallelism: Option[Int] = None,
                      threshold: Option[Int] = None,
                      timeout: Option[Int] = None,
                      exit: Option[Boolean] = None,
                      snapshot: Option[Boolean] = None
) derives Codec.AsObject

case class State(parameters: Parameters,
                 traces: Option[Traces] = None,
                 last: Option[Long] = None,
                 clock: Option[Double] = None,
                 idle: Option[Long] = None,
                 started: Option[Long] = None,
                 done: Option[Boolean] = None
) derives Codec.AsObject

case class Item(key: String,
                service: Consul.AgentService,
                id: Int,
                state: State,
                exit: Boolean,
                pause: Boolean,
                stop: Boolean,
                traces: Boolean,
                amazonsqs: Item.AmazonSQS,
                kafka: Item.Kafka,
                rabbitmq: Item.RabbitMQ,
                tooltip: Boolean)

object Item:

  def apply(key: String,
            service: Consul.AgentService,
            id: Int,
            signal: SignallingRef[IO, Boolean],
            state: State,
            exit: Boolean,
            pause: Boolean,
            stop: Boolean,
            traces: Boolean): Item =
    Item(key, service, id, state, exit, pause, stop, traces, AmazonSQS(), Kafka(), RabbitMQ(signal), false)

  case class AmazonSQS(region: String = "elasticmq",
                       accessKey: String = "x",
                       secretKey: String = "x",
                       sessionToken: String = "feedback",
                       //endpoint: String = "http://localhost:9324",
                       endpoint: String = "http://localhost:5173",
                       //endpoint: String = "http://localhost:5173/sqs-proxy",
                       limit: Int = 10,
                       timeout: Int = 3,
                       receive: Boolean = false)

  case class Kafka(offset: Long = 0L,
                   maxBytes: Int = 32768,
                   timeout: Int = 3000,
                   receive: Boolean = false)

  case class RabbitMQ(signal: SignallingRef[IO, Boolean],
                      username: String = "guest",
                      password: String = "guest",
                      url: String = "ws://localhost:15674/ws",
                      connect: Boolean = false)

  case class Props(key: String,
                   service: Consul.AgentService,
                   restore: StateSnapshot[Parameters],
                   state: StateSnapshot[State],
                   clock: StateSnapshot[Double],
                   done: StateSnapshot[Boolean],
                   parallelism: StateSnapshot[Int],
                   threshold: StateSnapshot[Int],
                   timeout: StateSnapshot[Int],
                   snapshot: StateSnapshot[Boolean],
                   exit: StateSnapshot[Boolean],
                   pause: StateSnapshot[Boolean],
                   stop: StateSnapshot[Boolean],
                   traces: StateSnapshot[Boolean],
                   amazonsqs: StateSnapshot[AmazonSQS],
                   kafka: StateSnapshot[Kafka],
                   rabbitmq: StateSnapshot[RabbitMQ],
                   tooltip: StateSnapshot[Boolean])

  def Component(using Client[IO]) = ScalaFnComponent[Props] { p =>

    <.li(
      ^.listStyleType.none,
      ^.padding := "8px",
      ^.cursor := "pointer",

      <.label(^.htmlFor := "clock-number", "Clock: "),

      <.input(
        ^.marginRight := "15px",
        ^.id          := "clock-number",
        ^.`type`      := "number",
        ^.value       := p.clock.value,
        ^.size        := p.clock.value.toString.length,
        ^.disabled    := true
      ),

      <.label(^.htmlFor := "done-checkbox", "Done: "),

      <.input(
        ^.marginRight := "15px",
        ^.id          := "done-checkbox",
        ^.`type`      := "checkbox",
        ^.checked     := p.done.value,
        ^.disabled    := true
      ),

      <.button(
        ^.marginRight := "15px",
        ^.disabled    := p.pause.value,
        ^.onClick    --> p.service.parameters(p.restore.value).flatMap(p.state.setState(_).to[IO]),
        "Restore"
      ),

      <.label(^.htmlFor := "parallelism-number", "Parallelism: "),

      <.input(
        ^.marginRight := "15px",
        ^.id          := "parallelism-number",
        ^.`type`      := "number",
        ^.value       := p.parallelism.value,
        ^.size        := Int.MaxValue.toString.length,
        ^.disabled    := p.pause.value,
        ^.onChange   ==> { (e: ReactEventFromInput) => p.parallelism.setState(e.target.valueAsNumber.toInt) },
        ^.onBlur     ==> { (e: ReactEventFromInput) =>
          val params = Parameters(parallelism = Some(e.target.valueAsNumber.toInt))
          p.service.parameters(params).flatMap(p.state.setState(_).to[IO])
        },
      ),

      <.label(^.htmlFor := "batch-checkbox", "Batch: "),

      <.input(
        ^.marginRight := "15px",
        ^.id          := "batch-checkbox",
        ^.`type`      := "checkbox",
        ^.checked     := p.threshold.value > 0,
        ^.disabled    := true
      ),

      <.label(^.htmlFor := "threshold-number", "Threshold: "),

      <.input(
        ^.marginRight := "15px",
        ^.id          := "threshold-number",
        ^.`type`      := "number",
        ^.value       := p.threshold.value,
        ^.size        := Int.MaxValue.toString.length,
        ^.disabled    := p.pause.value,
        ^.onChange   ==> { (e: ReactEventFromInput) => p.threshold.setState(e.target.valueAsNumber.toInt) },
        ^.onBlur     ==> { (e: ReactEventFromInput) =>
          val params = Parameters(threshold = Some(e.target.valueAsNumber.toInt))
          p.service.parameters(params).flatMap(p.state.setState(_).to[IO])
        },
      ),

      <.label(^.htmlFor := "timeout-number", "Timeout: "),

      <.input(
        ^.marginRight := "15px",
        ^.id          := "timeout-number",
        ^.`type`      := "number",
        ^.value       := p.timeout.value,
        ^.size        := Int.MaxValue.toString.length,
        ^.disabled    := p.pause.value,
        ^.onChange   ==> { (e: ReactEventFromInput) => p.timeout.setState(e.target.valueAsNumber.toInt) },
        ^.onBlur     ==> { (e: ReactEventFromInput) =>
          val params = Parameters(timeout = Some(e.target.valueAsNumber.toInt))
          p.service.parameters(params).flatMap(p.state.setState(_).to[IO])
        },
      ),

      <.label(^.htmlFor := "snapshot-checkbox", "Snapshot: "),

      <.input(
        ^.marginRight := "15px",
        ^.id          := "snapshot-checkbox",
        ^.`type`      := "checkbox",
        ^.checked     := p.snapshot.value,
        ^.disabled    := p.pause.value,
        ^.onChange   ==> { (e: ReactEventFromInput) =>
          val params = Parameters(snapshot = Some(e.target.checked))
          p.service.parameters(params).flatMap(p.state.setState(_).to[IO])
        },
      ),

      <.button(
        ^.marginRight := "15px",
        ^.onClick    --> p.service.state.flatMap(p.state.setState(_).to[IO]),
        "🔄"
      ),

      <.div(
        ^.position.relative,
        ^.display.inlineBlock,
        ^.onMouseOver --> p.tooltip.setState(true).to[IO],
        ^.onMouseLeave --> p.tooltip.setState(false).to[IO],

        <.span(p.service.Address + ":" + p.service.Port),

        if p.tooltip.value
        then
          <.div(
            ^.position.absolute,
            ^.bottom          := "100%",
            ^.left            := "50%",
            ^.transform       := "translateX(-25%)",
            ^.backgroundColor := "black",
            ^.color           := "white",
            ^.padding         := "5px",
            ^.borderRadius    := "4px",
            ^.whiteSpace.nowrap,
            ^.zIndex          := "100",
            p.key
          )
        else
          VdomArray.empty()
      ),

      <.input(
        ^.marginLeft := "15px",
        ^.`type`     := "checkbox",
        ^.checked    := (if p.stop.value then false else p.pause.value),
        ^.disabled   := p.stop.value,
        ^.onChange  ==> { (e: ReactEventFromInput) =>
          p.service.pause(e.target.checked).flatMap(p.pause.setState(_).to[IO])
        },
      ),

      <.span(
        ^.marginLeft := "8px",
        "Pause"
      ),

      <.input(
        ^.marginLeft := "15px",
        ^.`type`     := "checkbox",
        ^.checked    := (if p.stop.value then false else p.traces.value),
        ^.disabled   := p.stop.value,
        ^.onChange  ==> { (e: ReactEventFromInput) =>
          p.service.traces(e.target.checked).flatMap(p.traces.setState(_).to[IO])
        },
      ),

      <.span(
        ^.marginLeft := "8px",
        "Traces"
      ),

      <.input(
        ^.marginLeft := "15px",
        ^.`type`     := "checkbox",
        ^.checked    := p.stop.value,
        ^.disabled   := p.stop.value,
        ^.onChange  --> {
          for
            _ <- p.stop.setState(true).to[IO]
            _ <- p.service.traces(false)
            _ <- p.service.stop(true)
          yield
            ()
        },
      ),

      <.span(
        ^.marginLeft := "8px",
        "Stop"
      ),

      <.input(
        ^.marginLeft := "15px",
        ^.`type`     := "checkbox",
        ^.checked    := p.exit.value,
        ^.disabled   := p.stop.value && p.exit.value,
        ^.onChange  ==> { (e: ReactEventFromInput) =>
          p.service.exit(e.target.checked).flatMap(p.exit.setState(_).to[IO])
        },
      ),

      <.span(
        ^.marginLeft := "8px",
        "Exit"
      ),

      p.state.value.traces.get match {

        case Traces.AmazonSQS("elasticmq", queue) if !p.stop.value =>
          <.div(

            <.input(
              ^.id        := "region-text",
              ^.`type`    := "text",
              ^.value     := p.amazonsqs.value.region,
              ^.onChange ==> { (e: ReactEventFromInput) => p.amazonsqs.modState(_.copy(region = e.target.value)) },
            ),

            <.span(
              ^.marginLeft := "8px",
              "Region"
            ),

            <.input(
              ^.marginLeft := "15px",
              ^.id         := "accessKey-text",
              ^.`type`     := "text",
              ^.value      := p.amazonsqs.value.accessKey,
              ^.onChange  ==> { (e: ReactEventFromInput) => p.amazonsqs.modState(_.copy(accessKey = e.target.value)) },
            ),

            <.span(
              ^.marginLeft := "8px",
              "Access Key"
            ),

            <.input(
              ^.marginLeft := "15px",
              ^.id         := "secretKey-text",
              ^.`type`     := "text",
              ^.value      := p.amazonsqs.value.secretKey,
              ^.onChange  ==> { (e: ReactEventFromInput) => p.amazonsqs.modState(_.copy(secretKey = e.target.value)) },
            ),

            <.span(
              ^.marginLeft := "8px",
              "Secret Key"
            ),

            <.input(
              ^.marginLeft := "15px",
              ^.id         := "limit-number",
              ^.`type`     := "number",
              ^.value      := p.amazonsqs.value.limit,
              ^.onChange  ==> { (e: ReactEventFromInput) => p.amazonsqs.modState(_.copy(limit = 10 min (1 max e.target.valueAsNumber.toInt.abs))) },
            ),

            <.span(
              ^.marginLeft := "8px",
              "Limit"
            ),

            <.input(
              ^.marginLeft := "15px",
              ^.id         := "timeout-number",
              ^.`type`     := "number",
              ^.value      := p.amazonsqs.value.timeout,
              ^.onChange  ==> { (e: ReactEventFromInput) => p.amazonsqs.modState(_.copy(timeout = 3 max e.target.valueAsNumber.toInt.abs)) },
            ),

            <.span(
              ^.marginLeft := "8px",
              "Timeout"
            ),

            <.input(
              ^.marginLeft := "15px",
              ^.id         := "receive-checkbox",
              ^.`type`     := "checkbox",
              ^.checked    := p.amazonsqs.value.receive,
              ^.onChange  ==> { (e: ReactEventFromInput) => p.amazonsqs.modState(_.copy(receive = e.target.checked)) }
            ),

            <.span(
              ^.marginLeft := "8px",
              "Receive"
            ),

            if p.amazonsqs.value.receive
            then
              val queueUrl = s"${p.amazonsqs.value.endpoint}/queue/$queue"
              val AmazonSQS(region, accessKey, secretKey, token, _, limit, timeout, _) = p.amazonsqs.value
              <.div(amazonsqs.AmazonSQSReceiver(queueUrl, region, accessKey, secretKey, token, limit, timeout).Component())
            else
              VdomArray.empty()

          )

        case Traces.Kafka("redpanda", topic) if !p.stop.value =>

          <.div(

            <.input(
              ^.id        := "offset-number",
              ^.`type`    := "number",
              ^.value     := p.kafka.value.offset,
              ^.onChange ==> { (e: ReactEventFromInput) => p.kafka.modState(_.copy(offset = e.target.valueAsNumber.toLong)) },
            ),

            <.span(
              ^.marginLeft := "8px",
              "Offset"
            ),

            <.input(
              ^.marginLeft := "15px",
              ^.id         := "maxBytes-number",
              ^.`type`     := "number",
              ^.value      := p.kafka.value.maxBytes,
              ^.onChange  ==> { (e: ReactEventFromInput) => p.kafka.modState(_.copy(maxBytes = 1024 max e.target.valueAsNumber.toInt.abs)) },
            ),

            <.span(
              ^.marginLeft := "8px",
              "Maximum bytes"
            ),

            <.input(
              ^.marginLeft := "15px",
              ^.id         := "timeout-number",
              ^.`type`     := "number",
              ^.value      := p.kafka.value.timeout,
              ^.onChange  ==> { (e: ReactEventFromInput) => p.kafka.modState(_.copy(timeout = 300 max e.target.valueAsNumber.toInt.abs)) },
            ),

            <.span(
              ^.marginLeft := "8px",
              "Timeout"
            ),

            <.input(
              ^.marginLeft := "15px",
              ^.id         := "receive-checkbox",
              ^.`type`     := "checkbox",
              ^.checked    := p.kafka.value.receive,
              ^.onChange  ==> { (e: ReactEventFromInput) => p.kafka.modState(_.copy(receive = e.target.checked)) }
            ),

            <.span(
              ^.marginLeft := "8px",
              "Receive"
            ),

            if p.kafka.value.receive
            then
              <.div(kafka.redpanda.Component(kafka.redpanda.Props(topic, p.kafka.value.offset, p.kafka.value.maxBytes, p.kafka.value.timeout)))
            else
              VdomArray.empty()
          )

        case Traces.RabbitMQ(queue) if !p.stop.value =>

          <.div(

            <.input(
              ^.id        := "username-text",
              ^.`type`    := "text",
              ^.value     := p.rabbitmq.value.username,
              ^.onChange ==> { (e: ReactEventFromInput) => p.rabbitmq.modState(_.copy(username = e.target.value)) },
            ),

            <.span(
              ^.marginLeft := "8px",
              "Username"
            ),

            <.input(
              ^.marginLeft := "15px",
              ^.id         := "password-text",
              ^.`type`     := "text",
              ^.value      := p.rabbitmq.value.password,
              ^.onChange  ==> { (e: ReactEventFromInput) => p.rabbitmq.modState(_.copy(password = e.target.value)) },
            ),

            <.span(
              ^.marginLeft := "8px",
              "Password"
            ),

            <.input(
              ^.marginLeft := "15px",
              ^.id         := "interrupt-checkbox",
              ^.`type`     := "checkbox",
              ^.onChange  ==> { (e: ReactEventFromInput) => p.rabbitmq.value.signal.set(e.target.checked) }
            ),

            <.span(
              ^.marginLeft := "8px",
              "Interrupt"
            ),

            <.input(
              ^.marginLeft := "15px",
              ^.id         := "connect-checkbox",
              ^.`type`     := "checkbox",
              ^.checked    := p.rabbitmq.value.connect,
              ^.onChange  ==> { (e: ReactEventFromInput) => p.rabbitmq.modState(_.copy(connect = e.target.checked)) }
            ),

            <.span(
              ^.marginLeft := "8px",
              "Connect"
            ),

            if p.rabbitmq.value.connect
            then
              <.div(rabbitmq.Component(rabbitmq.Props(queue, p.rabbitmq.value.signal, p.rabbitmq.value.username, p.rabbitmq.value.password, p.rabbitmq.value.url)))
            else
              VdomArray.empty()
          )

        case _ =>
          VdomArray.empty()

      }

    )

  }


case class Output(items: List[Item] = Nil)

case class Restore(params: List[(Parameters, Int)] = Nil)


object Output:

  val defaultUrl = "http://localhost:8500"

  def Component(using Client[IO]) = ScalaFnComponent[(StateSnapshot[Output], StateSnapshot[Restore])] { (output, restore) =>

    <.div(
      <.ul(
        (output.value.items zip restore.value.params).map { (item, params) =>
          val lens: StateSnapshot[Item] = output.zoomState(_.items(item.id)) { i => o =>
            o.copy(items = o.items.take(i.id) ::: i :: o.items.drop(i.id + 1))
          }

          val lensʹ: StateSnapshot[(Parameters, Int)] = restore.zoomState(_.params(params._2)) { p => r =>
            r.copy(params = r.params.take(p._2) ::: p :: r.params.drop(p._2 + 1))
          }

          val state = lens.zoomStateL(Focus[Item](_.state))

          val clock = state.zoomState(_.clock.get) {
            c => s => s.copy(clock = Some(c))
          }

          val done = state.zoomState(_.done.get) {
            d => s => s.copy(done = Some(d))
          }

          val parallelism = state.zoomState(_.parameters.parallelism.get) {
            p => s => s.copy(parameters = s.parameters.copy(parallelism = Some(p)))
          }
          val threshold = state.zoomState(_.parameters.threshold.get) {
            h => s => s.copy(parameters = s.parameters.copy(threshold = Some(h)))
          }
          val timeout = state.zoomState(_.parameters.timeout.get) {
            t => s => s.copy(parameters = s.parameters.copy(timeout = Some(t)))
          }
          val snapshot = state.zoomState(_.parameters.snapshot.get) {
            o => s => s.copy(parameters = s.parameters.copy(snapshot = Some(o)))
          }

          Item.Component.withKey(item.key)(
            Item.Props(item.key,
                       item.service,
                       lensʹ.zoomStateL(Focus[(Parameters, Int)](_._1)),
                       state,
                       clock,
                       done,
                       parallelism,
                       threshold,
                       timeout,
                       snapshot,
                       lens.zoomStateL(Focus[Item](_.exit)),
                       lens.zoomStateL(Focus[Item](_.pause)),
                       lens.zoomStateL(Focus[Item](_.stop)),
                       lens.zoomStateL(Focus[Item](_.traces)),
                       lens.zoomStateL(Focus[Item](_.amazonsqs)),
                       lens.zoomStateL(Focus[Item](_.kafka)),
                       lens.zoomStateL(Focus[Item](_.rabbitmq)),
                       lens.zoomStateL(Focus[Item](_.tooltip)))
          )
        }.toTagMod
      )
    )

  }
