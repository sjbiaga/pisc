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

        <.option(^.value := "ce"  , "Cats Effect"          ).when(input.effects.selectedEffect == "cats.effect.IO"),
        <.option(^.value := "cef" , "Cats Effect (flatMap)").when(input.effects.selectedEffect == "cats.effect.IO"),
        <.option(^.value := "zio" , "ZIO"                  ).when(input.effects.selectedEffect == "zio.Task"),
        <.option(^.value := "ziof", "ZIO (flatMap)"        ).when(input.effects.selectedEffect == "zio.Task"),
        <.option(^.value := "fs2" , "Functional Streams 2" ),
        <.option(^.value := "zs"  , "ZIO Streams"          ).when(input.effects.selectedEffect == "zio.Task")
      )
    )

  }
