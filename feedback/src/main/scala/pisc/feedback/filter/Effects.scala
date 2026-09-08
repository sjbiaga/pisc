package pisc
package feedback
package filter

import cats.effect.IO

import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.EffectCatsEffect.*
import japgolly.scalajs.react.vdom.html_<^.*


object Effects:

  case class State(selectedEffect: String)

  val Component = ScalaFnComponent[(Input, String => IO[Unit])] { (input, cb) =>

    def onEffectsChange(e: ReactEventFromInput): IO[Unit] = cb(e.target.value)

    <.div(
      <.label(^.htmlFor  := "effect-select", "Effect: "),

      <.select(
        ^.id             := "effect-select",
        ^.value          := input.effects.selectedEffect,
        ^.onChange      ==> onEffectsChange,

        <.option(^.value := "cats.effect.IO", "Cats Effect IO"),
        <.option(^.value := "zio.Task"      , "ZIO Task"      )
      )
    )

  }
