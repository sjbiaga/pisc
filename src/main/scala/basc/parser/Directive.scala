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

package basc
package parser

import java.nio.file.Path
import java.net.URI

import scala.collection.mutable.{
  LinkedHashMap => Map,
  LinkedHashSet => Set
}

import scala.meta.{ Lit, Term }
import emitter.shared.Meta.\

import BioAmbients.Emitter
import Directive.*


case class Directive(directive: (String, String | List[String]), emitter: Emitter, settings: Settings):

  import Directive.Settings.*
  import Traces.*
  import Uri.*

  implicit val name: String = directive._1.toLowerCase

  val self = directive._2

  private def canonical: String => String =
    case "werr"      => "errors"
    case "dups"      => "duplications"
    case "params"
       | "param"
       | "parameter" => "parameters"
    case it          => it

  private def key: String => Boolean = canonical andThen {
    case "echo"
       | "errors" | "duplications"
       | "exclude" | "include"
       | "paceunit"
       | "scaling"
       | "replication"
       | "typeclasses"
       | "parameters"
       | "traces"      => true
    case _             => false
  }

  private implicit def ?[S, T](fun: S => T): S ?=> T = { it ?=> fun(it) }

  given `_String | List[String]_`: {} with

    extension (self: String | List[String])
              (using err: String => ((String, String | List[String])) ?=> Throwable = { msg => dir ?=> DirectiveValueParsingException(dir, msg) })
              (using key: String)
              (using dir: (String, String | List[String]) = key -> self)

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

      def file: Option[String] =
        self match
          case it: String if it.toLowerCase == "console"                  => None
          case it: String if (try { Path.of(it); true } catch _ => false) => Some(s(it))
          case _                                                          => throw err("<console> or a filename")

      def string(`type`: String = "a string"): String =
        self match
          case it: String
              if (it.startsWith("\"") || it.startsWith("'"))
              && it.endsWith(s"${it.charAt(0)}")
              && it.length >= 2                              =>
            it.substring(1, it.length-1)
          case _                                             => throw err(`type`)

      def uri[F[_]: Parse](defaultPort: Int, cluster: Boolean = false): F[Config[Host, Port]] | Config[Hosts[F], Ports[F]] =
        self match
          case it: String                                                     =>
            val uri = URI(s(it))
            if cluster
            then
              Parse[F].inner(defaultPort)(uri.getScheme, uri.getHost, uri.getPort.toString, uri.getPath)
            else
              Parse[F].outer(defaultPort)(uri.getScheme, uri.getHost, uri.getPort.toString, uri.getPath)
          case List(it: String)                                               =>
            val uri = URI(s(it))
            if cluster
            then
              Parse[F].inner(defaultPort)(uri.getScheme, uri.getHost, uri.getPort.toString, uri.getPath)
            else
              Parse[F].outer(defaultPort)(uri.getScheme, uri.getHost, uri.getPort.toString, uri.getPath)
          case List(host: String, port: String)                               =>
            if cluster
            then
              Parse[F].inner(defaultPort)(null, s(host), s(port))
            else
              Parse[F].outer(defaultPort)(null, s(host), s(port))
          case List(scheme: String, host: String, port: String)               =>
            if cluster
            then
              Parse[F].inner(defaultPort)(s(scheme), s(host), s(port))
            else
              Parse[F].outer(defaultPort)(s(scheme), s(host), s(port))
          case List(scheme: String, host: String, port: String, path: String) =>
            if cluster
            then
              Parse[F].inner(defaultPort)(s(scheme), s(host), s(port), s(path))
            else
              Parse[F].outer(defaultPort)(s(scheme), s(host), s(port), s(path))
          case _                                                              => throw err("a [scheme://]host[:port[/path]] URI")

      def emitters: List[Emitter] =
        self match
          case it: String => List(Emitter.valueOf(it.toLowerCase))
          case it: List[String] => it.map(_.toLowerCase).map(Emitter.valueOf(_))

      def keys: Set[String] =
        self match
          case it: String if Directive.this.key(it.toLowerCase)                     => Set(canonical(it.toLowerCase))
          case it: List[String] if it.map(_.toLowerCase).forall(Directive.this.key) => Set.from(it.map(_.toLowerCase).map(canonical))
          case _                                                                    => throw err("a comma separated list of valid keys")

  private def boolean: Boolean = self.boolean
  private def number: Int = self.number
  private def file: Option[String] = self.file
  private def string(`type`: String = "a string"): String = self.string(`type`)
  //private def uri[F[_]: Parse](defaultPort: Int, cluster: Boolean = false): F[Config[Host, Port]] | Config[Hosts[F], Ports[F]] = self.uri[F](defaultPort, cluster)
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
            case List(given String, it: String)
                if given_String == "parallelism" =>
              (-1 max it.number(using { msg => DirectiveSettingParsingException(directive._1, _, msg) }), settings.replication._2)
            case List(given String, it: String)
                if given_String == "linear"      =>
              (settings.replication._1, it.boolean(using { msg => DirectiveSettingParsingException(directive._1, _, msg) }))
            case _                               => throw DirectiveValueParsingException(directive, message)
          case _                => throw DirectiveValueParsingException(directive, message)

      case "typeclasses" if settings.exclude =>

      case "typeclasses"  =>
        settings.typeclasses = self match
          case it: String       => List(it)
          case it: List[String] => it

      case "parameters"        =>
        settings.parameters = self match
          case it: List[String] => it.map(_.toLowerCase) match
            case List(given String, it: String)
                if given_String == "parallelism" =>
              settings.parameters.copy(parallelism = 1 max it.number(using { msg => DirectiveSettingParsingException(directive._1, _, msg) }))
            case List(given String, it: String)
                if given_String == "threshold"   =>
              settings.parameters.copy(threshold = 0 max it.number(using { msg => DirectiveSettingParsingException(directive._1, _, msg) }))
            case List(given String, it: String)
                if given_String == "timeout"     =>
              settings.parameters.copy(timeout = 0 max it.number(using { msg => DirectiveSettingParsingException(directive._1, _, msg) }))
            case List(given String, it: String)
                if given_String == "exit"        =>
              settings.parameters.copy(exit = it.boolean(using { msg => DirectiveSettingParsingException(directive._1, _, msg) }))
            case List(given String, it: String)
                if given_String == "snapshot"    =>
              settings.parameters.copy(snapshot = it.boolean(using { msg => DirectiveSettingParsingException(directive._1, _, msg) }))
            case _                               => throw DirectiveValueParsingException(directive, message)
          case _                => throw DirectiveValueParsingException(directive, message)

      case "traces"       =>
        try
          if boolean
          then
            settings.traces = Some(ConsoleCSV)
          else
            settings.traces = None
        catch _ =>
          val `type` = "a boolean, <console> or a filename, or " + message(using "traces")
          try
            self match
              case _: String                                 =>
                settings.traces = Some(file.fold(ConsoleCSV)(FileCSV.apply))
              // case given String :: (topic: String) :: it
              //     if given_String.toLowerCase == "kafka"     =>
              //   val hp = it.uri(using { msg => DirectiveSettingParsingException(directive._1, _, msg) })[List](9092, true).cluster._2
              //   settings.traces = Some(Kafka(hp.name, hp.number, s(topic)))
              case given String :: (queue: String) :: it
                  if given_String.toLowerCase == "rabbitmq"  =>
                val hp = it.uri(using { msg => DirectiveSettingParsingException(directive._1, _, msg) })[Id](5672).node._2
                settings.traces = Some(RabbitMQ(hp.name, hp.number, s(queue)))
              case given String :: (queue: String) :: it
                  if given_String.toLowerCase == "elasticmq" =>
                val (scheme, hp, _) = it.uri(using { msg => DirectiveSettingParsingException(directive._1, _, msg) })[Id](9324).node
                settings.traces = Some(ElasticMQ(scheme.getOrElse("http") -> hp, queue))
              case given String :: (queue: String) :: (region: String) :: (accessKey: String) :: (secretKey: String) :: it
                  if given_String.toLowerCase == "amazonsqs" =>
                val (scheme, hp, _) = it.uri(using { msg => DirectiveSettingParsingException(directive._1, _, msg) })[Id](443).node
                val schemeʹ = scheme.getOrElse(if hp.number == 443 then "https" else "http")
                assert(schemeʹ == "https" && hp.number == 443 || schemeʹ == "http" && hp.number == 80)
                settings.traces = Some(AmazonSQS(schemeʹ -> hp, s(region), s(accessKey), s(secretKey), s(queue)))
              case _                                         => throw DirectiveValueParsingException(directive, `type`)
          catch _ =>
            throw DirectiveValueParsingException(directive, `type`)

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
                                  "typeclasses"  -> settings.typeclasses,
                                  "parameters"   -> settings.parameters,
                                  "traces"       -> settings.traces)
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
              case it @ "parameters"     => it -> settings.parameters
              case it @ "traces"         => it -> settings.traces
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
            case ("parmeters", it: Parameters)       => settings.parameters = it
            case ("traces", it: Option[Traces])      => settings.traces = it
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
                      var typeclasses: List[String] = Nil,
                      var parameters: Settings.Parameters = Parameters(),
                      var traces: Option[Settings.Traces] = None)

  object Settings:

    case class Parameters(parallelism: Int = Int.MaxValue,
                          threshold: Int = 0,
                          timeout: Int = 123456,
                          exit: Boolean = true,
                          snapshot: Boolean = false):
      lazy val reify: Term = Term.Apply(\("Π-Parameters"), Term.ArgClause(Lit.Int(parallelism)
                                                                       :: Lit.Int(threshold)
                                                                       :: Lit.Int(timeout)
                                                                       :: Lit.Boolean(exit)
                                                                       :: Lit.Boolean(snapshot)
                                                                       :: Nil))

    trait Traces:
      lazy val reify: Term

    object Traces:

      import Uri.*

      case object ConsoleCSV extends Traces:
        lazy val reify = \("Π-ConsoleCSV")

      case class FileCSV(filename: String) extends Traces:
        lazy val reify = Term.Apply(\("Π-FileCSV"), Term.ArgClause(Lit.String(filename) :: Nil))

      case class Kafka(name: List[String], number: List[Int], topic: String) extends Traces with Hosts[List] with Ports[List]:
        require(name.size == number.size)
        lazy val reify = Term.Apply(\("Π-Kafka"), Term.ArgClause(Term.Apply(\("List"),
                                                                            Term.ArgClause((name zip number).map(_ + ":" + _).map(Lit.String(_))))
                                                              :: Lit.String(topic) :: Nil))

      case class RabbitMQ(name: String, number: Int, queue: String) extends Traces with Host with Port:
        lazy val reify = Term.Apply(\("Π-RabbitMQ"), Term.ArgClause(Lit.String(name) :: Lit.Int(number) :: Lit.String(queue) :: Nil))

      case class AmazonSQS(endpoint: (String, Host & Port), region: String, accessKey: String, secretKey: String, queue: String) extends Traces:
        lazy val reify = Term.Apply(\("Π-AmazonSQS"), Term.ArgClause(Lit.String(s"${endpoint._1}://${endpoint._2.name}:${endpoint._2.number}")
                                                                  :: Lit.String(region)
                                                                  :: Lit.String(accessKey)
                                                                  :: Lit.String(secretKey)
                                                                  :: Lit.String(queue)
                                                                  :: Nil))

      class ElasticMQ(endpoint: (String, Host & Port), queue: String) extends AmazonSQS(endpoint, "elasticmq", "x", "x", queue)

    object Uri:

      type Id[X] = X

      type Config[H <: Hosts[?], P <: Ports[?]] = (Option[String], H & P, String)

      extension [F[_]](self: F[Config[Host, Port]] | Config[Hosts[F], Ports[F]])
        def cluster = self.asInstanceOf[Config[Hosts[F], Ports[F]]]
        def node = self.asInstanceOf[F[Config[Host, Port]]]

      trait Parse[F[_]]:
        def outer(defaultPort: Int)(scheme: String, host: String, port: String, path: String = null): F[(Option[String], Host & Port, String)]
        def inner(defaultPort: Int)(scheme: String, host: String, port: String, path: String = null): (Option[String], Hosts[F] & Ports[F], String)

      object Parse:
        inline def apply[F[_]](using Parse[F]): Parse[F] = summon[Parse[F]]
        private def apply(defaultPort: Int)(uri: URI) =
          val portʹ = if uri.getPort == -1 then defaultPort else uri.getPort
          (Option(uri.getScheme), new Hostʹ(uri.getHost) with Portʹ(portʹ), uri.getPath)
        given Parse[Id] with
          def outer(defaultPort: Int)(scheme: String, host: String, port: String, path: String) =
            Parse(defaultPort)(URI(scheme, null, host, port.toInt, path, null, null))
          def inner(defaultPort: Int)(scheme: String, host: String, port: String, path: String) =
            outer(defaultPort)(scheme, host, port, path)
        given Parse[List] with
          def outer(defaultPort: Int)(scheme: String, host: String, port: String, path: String) =
            (host.split(",") zip port.split(",").map(_.toInt)).map(URI(scheme, null, _, _, path, null, null)).map(Parse.apply(defaultPort)).toList
          def inner(defaultPort: Int)(scheme: String, host: String, port: String, path: String) =
            val hosts = host.split(",").toList
            val ports = port.split(",").map(_.toInt).map { it => if it == -1 then defaultPort else it }.toList
            val uri = URI(scheme, null, hosts.head, ports.head, path, null, null)
            (Option(uri.getScheme), new Hostsʹ(hosts) with Portsʹ(ports), uri.getPath)

      trait Hosts[F[_]]:
        val name: F[String]

      trait Hostsʹ[F[_]](override val name: F[String]) extends Hosts[F]

      trait Host extends Hosts[Id]

      trait Hostʹ(override val name: String) extends Host

      trait Ports[F[_]]:
        val number: F[Int]

      trait Portsʹ[F[_]](override val number: F[Int]) extends Ports[F]

      trait Port extends Ports[Id]

      trait Portʹ(override val number: Int) extends Port

    private lazy val messages = Map("replication" -> "a <parallelism> number or a <linear> boolean setting",
                                    "batch"       -> "a <threshold> number or a <timeout> number setting",
                                    "traces"      -> "a <Kafka> cluster config or a <RabbitMQ> config or an <AmazonSQS> client config or an <ElasticMQ> endpoint setting")
    def message(implicit name: String) = messages(name)

    def s(it: String): String =
      if (it.startsWith("\"") || it.startsWith("'"))
      && it.endsWith(s"${it.charAt(0)}")
      && it.length >= 2
      then it.substring(1, it.length-1)
      else it


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
