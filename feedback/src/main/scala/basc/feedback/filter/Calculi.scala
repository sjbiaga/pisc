package basc
package feedback
package filter

import cats.effect.IO

import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.EffectCatsEffect.*
import japgolly.scalajs.react.vdom.html_<^.*


enum Calculi(val tag: String):
  case stochasticpicalculus extends Calculi("StochasticPiCalculus")
  case bioambients extends Calculi("BioAmbients")


object Calculi:

  case class State(selectedCalculus: String)

  val Component = ScalaFnComponent[(Input, String => IO[Unit])] { (input, cb) =>

    def onCalculiChange(e: ReactEventFromInput): IO[Unit] = cb(e.target.value)

    <.div(
      <.label(^.htmlFor  := "calculus-select", "Calculus: "),

      <.select(
        ^.id             := "calculus-select",
        ^.value          := input.calculi.selectedCalculus,
        ^.onChange      ==> onCalculiChange,

        <.option(^.value := "stochasticpicalculus", "Stochastic π-Calculus"),
        <.option(^.value := "bioambients"         , "BioAmbients")
      )
    )

  }
