package basc
package feedback
package filter

import cats.effect.IO

import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.EffectCatsEffect.*
import japgolly.scalajs.react.vdom.html_<^.*


object Emitters:

  case class State(selectedEmitter: String)

  val Component = ScalaFnComponent[(Input, String => IO[Unit])] { (input, cb) =>

    def onEmittersChange(e: ReactEventFromInput): IO[Unit] = cb(e.target.value)

    <.div(
      <.label(^.htmlFor  := "emitter-select", "Emitter: "),

      <.select(
        ^.id             := "emitter-select",
        ^.value          := input.emitters.selectedEmitter,
        ^.onChange      ==> onEmittersChange,

        <.option(^.value := "ce"  , "Cats Effect"          ),
        <.option(^.value := "cef" , "Cats Effect (flatMap)"),
        <.option(^.value := "zio" , "ZIO"                  ),
        <.option(^.value := "ziof", "ZIO (flatMap)"        )
      )
    )

  }
