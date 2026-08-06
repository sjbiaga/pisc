/*
 * Copyright (c) 2023-2026 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * [Except as contained in this notice, the name of Sebastian I. Gliţa-Catina
 * shall not be used in advertising or otherwise to promote the sale, use
 * or other dealings in this Software without prior written authorization
 * from Sebastian I. Gliţa-Catina.]
 */

package pisc
package parser

import scala.collection.mutable.{
  LinkedHashMap => Map,
  LinkedHashSet => Set
}

import PolyadicPi.Emitter
import Directive.*


case class Directive(directive: (String, String | List[String]), emitter: Emitter, settings: Settings):

  implicit val name: String = directive._1.toLowerCase

  val self = directive._2

  private def canonical: String => String =
    case "werr" => "errors"
    case "dups" => "duplications"
    case it     => it

  private def key: String => Boolean = canonical andThen {
    case "echo"
       | "errors" | "duplications"
       | "exclude" | "include"
       | "paceunit"
       | "scaling"
       | "replication"
       | "typeclasses" => true
    case _             => false
  }

  private implicit def ?[S, T](fun: S => T): S ?=> T = { it ?=> fun(it) }

  extension (self: String | List[String])
            (using err: String => ((String, String | List[String])) ?=> Throwable = { msg => dir ?=> DirectiveValueParsingException(dir, msg) })
            (using dir: String ?=> (String, String | List[String]) = { key ?=> key -> self })
            (using key: String)

    def boolean: Boolean =
      self match
        case it: String =>
          it.toLowerCase match
            case "0" | "off" | "false" | "no" | "n" => false
            case "1" | "on" | "true" | "yes" | "y"  => true
            case _                                  => throw err("a boolean")
        case _          => throw err("a boolean")

    def number: Int =
      self match
        case it: String =>
          try
            it.toInt
          catch
            case _: NumberFormatException =>
              throw err("a number")
        case _          => throw err("a number")

    def emitters: List[Emitter] =
      self match
        case it: String => List(Emitter.valueOf(it.toLowerCase))
        case it: List[String] => it.map(_.toLowerCase).map(Emitter.valueOf(_))

    def keys: Set[String] =
      self match
        case it: String if this.key(it)              => Set(canonical(it))
        case it: List[String] if it.forall(this.key) => Set.from(it.map(canonical))
        case _                                       => throw err("a comma separated list of valid keys")

  private def boolean: Boolean = self.boolean
  private def number: Int = self.number
  private def emitters: List[Emitter] = self.emitters
  private def keys: Set[String] = self.keys

  def apply(): Unit =

    canonical(name) match

      case "echo" if settings.exclude =>
        Console.err.println(self)

      case "echo"         =>
        Console.println(self)

      case "errors"       =>
        settings.werr = boolean

      case "duplications" =>
        settings.dups = boolean

      case "exclude"      =>
        try
          settings.exclude = boolean
        catch _ =>
          try
            settings.exclude = emitters.contains(emitter)
          catch _ =>
            throw DirectiveValueParsingException(directive, "a boolean or emitter(s)")

      case "include"      =>
        try
          settings.exclude = !boolean
        catch _ =>
          try
            settings.exclude = !emitters.contains(emitter)
          catch _ =>
            throw DirectiveValueParsingException(directive, "a boolean or emitter(s)")

      case "paceunit"     =>
        settings.paceunit = self match
          case it: String => it
          case _          => throw DirectiveValueParsingException(directive, "a time unit")

      case "scaling"      =>
        settings.scaling = boolean

      case "replication"  =>
        settings.replication = self match
          case it: List[String] => it.map(_.toLowerCase) match
            case List(given String: "parallelism", it: String) =>
              (-1 max it.number(using { msg => DirectiveSettingParsingException(directive._1, _, msg) }), settings.replication._2)
            case List(given String: "linear", it: String)      =>
              (settings.replication._1, it.boolean(using { msg => DirectiveSettingParsingException(directive._1, _, msg) }))
            case _                               => throw DirectiveValueParsingException(directive, settings.message)
          case _                => throw DirectiveValueParsingException(directive, settings.message)

      case "typeclasses" if settings.exclude =>

      case "typeclasses"  =>
        settings.typeclasses = self match
          case it: String       => List(it)
          case it: List[String] => it

      case "push"         =>
        try
          if boolean
          then
            settings.dirs ::= Map("echo"         -> (),
                                  "errors"       -> settings.werr,
                                  "duplications" -> settings.dups,
                                  "exclude"      -> settings.exclude,
                                  "paceunit"     -> settings.paceunit,
                                  "scaling"      -> settings.scaling,
                                  "replication"  -> settings.replication,
                                  "typeclasses"  -> settings.typeclasses)
        catch _ =>
          settings.dirs ::= Map.from {
            keys.map {
              case it @ "echo"           => it -> ()
              case it @ "errors"         => it -> settings.werr
              case it @ "duplications"   => it -> settings.dups
              case "exclude" | "include" => "exclude" -> settings.exclude
              case it @ "paceunit"       => it -> settings.paceunit
              case it @ "scaling"        => it -> settings.scaling
              case it @ "replication"    => it -> settings.replication
              case it @ "typeclasses"    => it -> settings.typeclasses
            }
          }

      case "pop"          =>
        if boolean
        then
          settings.dirs.head.foreach {
            case ("echo", _)                         =>
            case ("errors", it: Boolean)             => settings.werr = it
            case ("duplications", it: Boolean)       => settings.dups = it
            case ("exclude", it: Boolean)            => settings.exclude = it
            case ("paceunit", it: String)            => settings.paceunit = it
            case ("scaling", it: Boolean)            => settings.scaling = it
            case ("replication", it: (Int, Boolean)) => settings.replication = it
            case ("typeclasses", it: List[String])   => settings.typeclasses = it
            case _                                   => ???
          }
          settings.dirs = settings.dirs.tail

        if settings.dirs.isEmpty
        then
          Directive("push" -> "1", emitter, settings)()

      case _              => throw DirectiveKeyParsingException(directive)


object Directive:

  case class Settings(var dirs: List[Map[String, Any]] = Nil,
                      var werr: Boolean = false,
                      var dups: Boolean = false,
                      var exclude: Boolean = false,
                      var paceunit: String = "second",
                      var scaling: Boolean = false,
                      var replication: (Int, Boolean) = (-1, false),
                      var typeclasses: List[String] = Nil):

    private lazy val messages = Map("replication" -> "a <parallelism> number or a <linear> boolean setting")
    def message(implicit name: String) = messages(name)


  abstract sealed class DirectiveParsingException(msg: String, cause: Throwable = null)
      extends Expression.ParsingException(msg, cause)

  private object DirectiveParsingException:
    def apply(dir: (String, String | List[String])): String =
      dir._2 match
        case it: String => s"⟦ ${dir._1} = $it ⟧"
        case it: List[String] => s"""⟦ ${dir._1} = ${it.mkString("(", ",", ")")} ⟧"""
    def apply(key: String, dir: (String, String | List[String])): String =
      dir._2 match
        case it: List[String] => s"""⟦ $key = (${dir._1}, ${it.mkString("(", ",", ")")}) ⟧"""
        case _ => s"⟦ $key = $dir ⟧"

  case class DirectiveKeyParsingException(dir: (String, String | List[String]))
      extends DirectiveParsingException(s"The key in the directive ${DirectiveParsingException(dir)} is not valid")

  case class DirectiveValueParsingException(dir: (String, String | List[String]), `type`: String)
      extends DirectiveParsingException(s"The value in the directive ${DirectiveParsingException(dir)} is not ${`type`}")

  case class DirectiveSettingParsingException(key: String, dir: (String, String | List[String]), `type`: String)
      extends DirectiveParsingException(s"The <${dir._1}> setting in the directive ${DirectiveParsingException(key, dir)} is not ${`type`}")
