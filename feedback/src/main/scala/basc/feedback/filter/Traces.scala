package basc
package feedback
package filter

import cats.effect.IO

import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.EffectCatsEffect.*
import japgolly.scalajs.react.vdom.html_<^.*


enum Traces(val service: Traces = Traces.same):
  case same
  case amazonsqs
  case elasticmq extends Traces(Traces.amazonsqs)
  case kafka
  case redpanda extends Traces(Traces.kafka)
  case rabbitmq


object Traces:

  case class State(selectedTraces: String)

  val Component = ScalaFnComponent[(Input, String => IO[Unit])] { (input, cb) =>

    def onTracesChange(e: ReactEventFromInput): IO[Unit] = cb(e.target.value)

    <.div(
      <.label(^.htmlFor  := "traces-select", "Traces: "),

      <.select(
        ^.id             := "traces-select",
        ^.value          := input.traces.selectedTraces,
        ^.onChange       ==> onTracesChange,

        <.option(^.value := "amazonsqs", "AmazonSQS"),
        <.option(^.value := "elasticmq", "ElasticMQ"),
        <.option(^.value := "kafka"    , "Kafka"    ),
        <.option(^.value := "redpanda" , "Redpanda" ),
        <.option(^.value := "rabbitmq" , "RabbitMQ" )
      )
    )

  }
