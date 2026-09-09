package basc
package feedback

import cats.effect.IO

import fs2.concurrent.SignallingRef

import io.circe.Codec
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client

import monocle.{ Focus, Lens }

import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.ReactMonocle.*
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

object Parameters:

  given Reusability[Parameters] = Reusability.by_==
  given Reusability[(Parameters, Int)] = Reusability.by_==


case class State(parameters: Parameters,
                 traces: Option[Traces] = None,
                 last: Option[Long] = None,
                 clock: Option[Double] = None,
                 idle: Option[Long] = None,
                 started: Option[Long] = None,
                 init: Option[Boolean] = None,
                 done: Option[Boolean] = None
) derives Codec.AsObject

object State:

  given Reusability[State] = Reusability.by_==


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

  given Reusability[Item] = Reusability.by(_.key)


  def apply(key: String,
            service: Consul.AgentService,
            id: Int,
            signal: SignallingRef[IO, Boolean],
            state: State,
            exit: Boolean,
            pause: Boolean,
            stop: Boolean,
            traces: Boolean): Item =
    Item(key,
         service,
         id,
         state,
         exit,
         pause,
         stop,
         traces,
         AmazonSQS.ElasticMQ(),
         Kafka.Redpanda(),
         RabbitMQ(signal),
         false)


  case class AmazonSQS(region: String,
                       accessKey: String,
                       secretKey: String,
                       endpoint: String,
                       sessionToken: String = "feedback",
                       limit: Int = 10,
                       timeout: Int = 3,
                       own: Boolean = false,
                       receive: Boolean = false)

  object AmazonSQS:

    given Reusability[AmazonSQS] = Reusability.by_==

    class ElasticMQ(endpoint: String = "http://localhost:5173") extends AmazonSQS("elasticmq", "x", "x", endpoint)


  case class Kafka(proxyUrl: String,
                   offset: Long = 0L,
                   maxBytes: Int = 32768,
                   timeout: Int = 3000,
                   own: Boolean = false,
                   receive: Boolean = false)

  object Kafka:

    class Redpanda(proxyUrl: String = "http://localhost:5173/redpanda-proxy") extends Kafka(proxyUrl)

    given Reusability[Kafka] = Reusability.by_==


  case class RabbitMQ(signal: SignallingRef[IO, Boolean],
                      username: String = "guest",
                      password: String = "guest",
                      url: String = "ws://localhost:15674/ws",
                      chunkSize: Int = 10,
                      own: Boolean = false,
                      connect: Boolean = false)

  object RabbitMQ:

    given Reusability[RabbitMQ] = Reusability.by { it =>
      (it.username, it.password, it.url, it.connect)
    }


  case class Props(key: String,
                   service: Consul.AgentService,
                   restore: StateSnapshot[Parameters],
                   _item: StateSnapshot[Item],
                   state: StateSnapshot[State],
                   clock: StateSnapshot[Double],
                   init: StateSnapshot[Boolean],
                   done: StateSnapshot[Boolean],
                   parallelism: StateSnapshot[Int],
                   threshold: StateSnapshot[Int],
                   timeout: StateSnapshot[Int],
                   snapshot: StateSnapshot[Option[Boolean]],
                   exit: StateSnapshot[Boolean],
                   pause: StateSnapshot[Boolean],
                   stop: StateSnapshot[Boolean],
                   traces: StateSnapshot[Boolean],
                   amazonsqs: StateSnapshot[AmazonSQS],
                   kafka: StateSnapshot[Kafka],
                   rabbitmq: StateSnapshot[RabbitMQ],
                   tooltip: StateSnapshot[Boolean])
                  (using val httpClient: Client[IO])

  object Props:

    given Reusability[Props] = Reusability.by(_._item)


  val Component = ScalaFnComponent.withReuse[Props] { p =>

    given Client[IO] = p.httpClient

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

      ( if p.service.isBioAmbients
        then
          <.div(
            ^.display.inlineBlock,

            <.label(^.htmlFor := "snapshot-checkbox", "Snapshot: "),

            <.input(
              ^.marginRight := "15px",
              ^.id          := "snapshot-checkbox",
              ^.`type`      := "checkbox",
              ^.checked     := p.snapshot.value.get,
              ^.disabled    := p.pause.value,
              ^.onChange   ==> { (e: ReactEventFromInput) =>
                val params = Parameters(snapshot = Some(e.target.checked))
                p.service.parameters(params).flatMap(p.state.setState(_).to[IO])
              },
            )
         )
        else
          <.div(^.display.inlineBlock)
      ),

      <.button(
        ^.marginRight := "15px",
        ^.disabled    := p.pause.value,
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
          <.div(^.display.inlineBlock, ^.position.absolute)
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

      ( if !p.init.value
        then
          <.div(
            ^.marginLeft := "15px",
            ^.className := "spinner",
            ^.display.inlineBlock,
            "Initializing..."
          )
        else
          <.div(^.display.inlineBlock)
      ),

      p.state.value.traces.get match {

        case Traces.AmazonSQS("elasticmq", queue) if !p.stop.value =>
          <.div(

            <.input(
              ^.id        := "region-text",
              ^.`type`    := "text",
              ^.disabled  := p.amazonsqs.value.receive,
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
              ^.disabled   := p.amazonsqs.value.receive,
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
              ^.disabled   := p.amazonsqs.value.receive,
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
              ^.disabled   := p.amazonsqs.value.receive,
              ^.value      := p.amazonsqs.value.limit,
              ^.onChange  ==> { (e: ReactEventFromInput) => p.amazonsqs.modState(_.copy(limit = 1 max e.target.valueAsNumber.toInt.abs)) },
            ),

            <.span(
              ^.marginLeft := "8px",
              "Limit"
            ),

            <.input(
              ^.marginLeft := "15px",
              ^.id         := "timeout-number",
              ^.`type`     := "number",
              ^.disabled   := p.amazonsqs.value.receive,
              ^.value      := p.amazonsqs.value.timeout,
              ^.onChange  ==> { (e: ReactEventFromInput) => p.amazonsqs.modState(_.copy(timeout = 3 max e.target.valueAsNumber.toInt.abs)) },
            ),

            <.span(
              ^.marginLeft := "8px",
              "Timeout"
            ),

            <.input(
              ^.marginLeft := "15px",
              ^.id         := "own-checkbox",
              ^.`type`     := "checkbox",
              ^.disabled   := p.amazonsqs.value.receive,
              ^.checked    := p.amazonsqs.value.own,
              ^.onChange  ==> { (e: ReactEventFromInput) => p.amazonsqs.modState(_.copy(own = e.target.checked)) }
            ),

            <.span(
              ^.marginLeft := "8px",
              "Own"
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
              val AmazonSQS(region, accessKey, secretKey, token, _, limit, timeout, own, _) = p.amazonsqs.value
              val receiver = amazonsqs.AmazonSQSReceiver(queueUrl, region, accessKey, secretKey, token, limit, timeout)
              val pid = if own then p.service.Meta.get("pid").toLong else -1
              <.div(amazonsqs.Component(amazonsqs.Props(p.key, p.service.isBioAmbients, pid, receiver)))
            else
              <.div

          )

        case Traces.Kafka("redpanda", topic) if !p.stop.value =>

          <.div(

            <.input(
              ^.id        := "offset-number",
              ^.`type`    := "number",
              ^.disabled  := p.kafka.value.receive,
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
              ^.disabled   := p.kafka.value.receive,
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
              ^.disabled   := p.kafka.value.receive,
              ^.value      := p.kafka.value.timeout,
              ^.onChange  ==> { (e: ReactEventFromInput) => p.kafka.modState(_.copy(timeout = 300 max e.target.valueAsNumber.toInt.abs)) },
            ),

            <.span(
              ^.marginLeft := "8px",
              "Timeout"
            ),

            <.input(
              ^.marginLeft := "15px",
              ^.id         := "own-checkbox",
              ^.`type`     := "checkbox",
              ^.disabled   := p.kafka.value.receive,
              ^.checked    := p.kafka.value.own,
              ^.onChange  ==> { (e: ReactEventFromInput) => p.kafka.modState(_.copy(own = e.target.checked)) }
            ),

            <.span(
              ^.marginLeft := "8px",
              "Own"
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
              val Kafka(proxyUrl, offset, maxBytes, timeout, own, _) = p.kafka.value
              val redpanda = kafka.redpanda.Redpanda(proxyUrl, topic, offset, maxBytes, timeout)
              val pid = if own then p.service.Meta.get("pid").toLong else -1
              <.div(kafka.redpanda.Component(kafka.redpanda.Props(p.key, p.service.isBioAmbients, pid, redpanda)))
            else
              <.div

          )

        case Traces.RabbitMQ(queue) if !p.stop.value =>

          <.div(

            <.input(
              ^.id        := "username-text",
              ^.`type`    := "text",
              ^.disabled  := p.rabbitmq.value.connect,
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
              ^.disabled   := p.rabbitmq.value.connect,
              ^.value      := p.rabbitmq.value.password,
              ^.onChange  ==> { (e: ReactEventFromInput) => p.rabbitmq.modState(_.copy(password = e.target.value)) },
            ),

            <.span(
              ^.marginLeft := "8px",
              "Password"
            ),

            <.input(
              ^.marginLeft := "15px",
              ^.id         := "chunkSize-number",
              ^.`type`     := "number",
              ^.disabled   := p.rabbitmq.value.connect,
              ^.value      := p.rabbitmq.value.chunkSize,
              ^.onChange  ==> { (e: ReactEventFromInput) => p.rabbitmq.modState(_.copy(chunkSize = 1 max e.target.valueAsNumber.toInt.abs)) },
            ),

            <.span(
              ^.marginLeft := "8px",
              "Chunk size"
            ),

            <.input(
              ^.marginLeft := "15px",
              ^.id         := "own-checkbox",
              ^.`type`     := "checkbox",
              ^.disabled   := p.rabbitmq.value.connect,
              ^.checked    := p.rabbitmq.value.own,
              ^.onChange  ==> { (e: ReactEventFromInput) => p.rabbitmq.modState(_.copy(own = e.target.checked)) }
            ),

            <.span(
              ^.marginLeft := "8px",
              "Own"
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

            if p.rabbitmq.value.connect
            then
              val RabbitMQ(signal, username, password, url, chunkSize, own, _) = p.rabbitmq.value
              val subscriber = rabbitmq.RabbitMQSubscriber(queue, username, password, url)
              val pid = if own then p.service.Meta.get("pid").toLong else -1
              <.div(rabbitmq.Component(rabbitmq.Props(p.key, p.service.isBioAmbients, chunkSize, pid, subscriber)(signal)))
            else
              <.div

          )

        case _ =>
          <.div

      }

    )

  }


case class Restore(params: List[(Parameters, Int)] = Nil)

object Restore:

  given Reusability[Restore] = Reusability.by_==


case class Output(items: List[Item] = Nil)


object Output:

  given Reusability[Double] = Reusability.by_==
  given Reusability[Output] = Reusability.by_==


  case class Props(output: StateSnapshot[Output],
                   restore: StateSnapshot[Restore])
                  (using val httpClient: Client[IO])

  object Props:

    given Reusability[Props] = Reusability.by { p => (p.output, p.restore) }


  val Component = ScalaFnComponent.withReuse[Props] { p =>

    given Client[IO] = p.httpClient

    <.div(
      <.ul(
        (p.output.value.items zip p.restore.value.params).map { (it, ps) =>
          val lens = Lens[Output, Item](_.items(it.id)) { i => o =>
            o.copy(items = o.items.take(i.id) ::: i :: o.items.drop(i.id + 1))
          }

          val item = StateSnapshot
            .withReuse
            .zoomL(lens)
            .prepare(p.output.toModStateFn)
            .apply(p.output.value)

          val lensʹ = Lens[Restore, (Parameters, Int)](_.params(ps._2)) { p => r =>
            r.copy(params = r.params.take(p._2) ::: p :: r.params.drop(p._2 + 1))
          }

          val paramsʹ = StateSnapshot
            .withReuse
            .zoomL(lensʹ)
            .prepare(p.restore.toModStateFn)
            .apply(p.restore.value)

          val params = StateSnapshot
            .withReuse
            .zoomL(Focus[(Parameters, Int)](_._1))
            .prepare(paramsʹ.toModStateFn)
            .apply(paramsʹ.value)

          val state = StateSnapshot
            .withReuse
            .zoomL(Focus[Item](_.state))
            .prepare(item.toModStateFn)
            .apply(item.value)

          val clock = StateSnapshot
            .withReuse
            .zoom[State, Double](_.clock.get) { c => _.copy(clock = Some(c)) }
            .prepare(state.toModStateFn)
            .apply(state.value)

          val init = StateSnapshot
            .withReuse
            .zoom[State, Boolean](_.init.get) { i => _.copy(init = Some(i)) }
            .prepare(state.toModStateFn)
            .apply(state.value)

          val done = StateSnapshot
            .withReuse
            .zoom[State, Boolean](_.done.get) { d => _.copy(done = Some(d)) }
            .prepare(state.toModStateFn)
            .apply(state.value)

          val parallelism = StateSnapshot
            .withReuse
            .zoom[State, Int](_.parameters.parallelism.get) { p => s => s.copy(parameters = s.parameters.copy(parallelism = Some(p))) }
            .prepare(state.toModStateFn)
            .apply(state.value)

          val threshold = StateSnapshot
            .withReuse
            .zoom[State, Int](_.parameters.threshold.get) { h => s => s.copy(parameters = s.parameters.copy(threshold = Some(h))) }
            .prepare(state.toModStateFn)
            .apply(state.value)

          val timeout = StateSnapshot
            .withReuse
            .zoom[State, Int](_.parameters.timeout.get) { t => s => s.copy(parameters = s.parameters.copy(timeout = Some(t))) }
            .prepare(state.toModStateFn)
            .apply(state.value)

          val snapshot = StateSnapshot
            .withReuse
            .zoom[State, Option[Boolean]](_.parameters.snapshot) { o => s => s.copy(parameters = s.parameters.copy(snapshot = o)) }
            .prepare(state.toModStateFn)
            .apply(state.value)

          val exit = StateSnapshot
            .withReuse
            .zoomL(Focus[Item](_.exit))
            .prepare(item.toModStateFn)
            .apply(item.value)

          val pause = StateSnapshot
            .withReuse
            .zoomL(Focus[Item](_.pause))
            .prepare(item.toModStateFn)
            .apply(item.value)

          val stop = StateSnapshot
            .withReuse
            .zoomL(Focus[Item](_.stop))
            .prepare(item.toModStateFn)
            .apply(item.value)

          val traces = StateSnapshot
            .withReuse
            .zoomL(Focus[Item](_.traces))
            .prepare(item.toModStateFn)
            .apply(item.value)

          val amazonsqs = StateSnapshot
            .withReuse
            .zoomL(Focus[Item](_.amazonsqs))
            .prepare(item.toModStateFn)
            .apply(item.value)

          val kafka = StateSnapshot
            .withReuse
            .zoomL(Focus[Item](_.kafka))
            .prepare(item.toModStateFn)
            .apply(item.value)

          val rabbitmq = StateSnapshot
            .withReuse
            .zoomL(Focus[Item](_.rabbitmq))
            .prepare(item.toModStateFn)
            .apply(item.value)

          val tooltip = StateSnapshot
            .withReuse
            .zoomL(Focus[Item](_.tooltip))
            .prepare(item.toModStateFn)
            .apply(item.value)

          Item.Component.withKey(it.key)(
            Item.Props(it.key,
                       it.service,
                       params,
                       item,
                       state,
                       clock,
                       init,
                       done,
                       parallelism,
                       threshold,
                       timeout,
                       snapshot,
                       exit,
                       pause,
                       stop,
                       traces,
                       amazonsqs,
                       kafka,
                       rabbitmq,
                       tooltip
            )
          )
        }.toTagMod
      )
    )

  }
