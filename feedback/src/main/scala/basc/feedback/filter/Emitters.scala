package basc
package feedback
package filter

import cats.effect.IO

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*


object Emitters:

  case class State(selectedEmitter: String)

  val Component = ScalaFnComponent[(Input, String => IO[Unit])] { (input, cb) =>

    def onEmittersChange(e: ReactEventFromInput): IO[Unit] = cb(e.target.value)

    val isIO = input.effects.selectedEffect == "cats.effect.IO" || input.effects.selectedEffect == "*"
    val isTask = input.effects.selectedEffect == "zio.Task" || input.effects.selectedEffect == "*"

    <.div(
      <.label(^.htmlFor  := "emitter-select", "Emitter: "),

      <.select(
        ^.id             := "emitter-select",
        ^.value          := input.emitters.selectedEmitter,
        ^.onChange      ==> onEmittersChange,

        <.option(^.value := "*"   , "*"                    ),
        <.option(^.value := "ce"  , "Cats Effect"          ).when(isIO),
        <.option(^.value := "cef" , "Cats Effect (flatMap)").when(isIO),
        <.option(^.value := "zio" , "ZIO"                  ).when(isTask),
        <.option(^.value := "ziof", "ZIO (flatMap)"        ).when(isTask),
        <.option(^.value := "fs2" , "Functional Streams 2" ),
        <.option(^.value := "zs"  , "ZIO Streams"          ).when(isTask)
      )
    )

  }
