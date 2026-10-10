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
package cef

import java.io.{ FileWriter, BufferedWriter }
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Paths

import scala.io.Source

import scala.meta.*
import dialects.Scala3

import com.comcast.ip4s.{ host, Host, IpAddress, Hostname }

import _root_.io.circe.syntax.*

import parser.StochasticPi
import parser.Calculus.{ `(*)`, λ }
import parser.e4s.Bind.given
import parser.e4s.Document
import parser.Directive.Settings.{ Elastic4s, Plugin }
import emitter.cef.Program
import emitter.cef.Meta.{ \, \\ }


object Main extends helper.Main:

  val examples = "examples"

  def main(args: Array[String]): Unit =
    var A: Host = host"localhost"
    var P = Int.MaxValue
    var H = 0
    var T = 123456
    var E = true
    var I = List.empty[Plugin]
    var J = false

    def bain(arg: String) =
      val in = if arg.endsWith(".pisc") then arg else arg + ".pisc"
      val out = Paths.get(s"$examples/in/", in.stripSuffix("pisc") + "scala.in").toString
      var source: Source = null
      var fwr: FileWriter = null
      var bwr: BufferedWriter = null

      val spi = StochasticPi.Main(StochasticPi.Emitter.cef, in, A.toString, P, H, T, E, I)

      try
        source = Source.fromFile(s"$examples/pisc/$in")
        fwr = FileWriter(out, UTF_8)
        bwr = BufferedWriter(fwr)

        val bs = spi(source)

        val bind = bs.zipWithIndex
        val prog_ = bind.filter(_._1.isRight).map(_.right.get -> _)

        val (prog, (discarded, excluded, enabled)) = spi(prog_.map(_._1))

        val ps =
          ( Defn.Val(Nil,
                     Pat.Var("π-AST") :: Nil,
                     Some(Type.Apply(\\("Option"), Type.ArgClause(\\("String") :: Nil))),
                     if J
                     then Term.Apply(\("Some"), Term.ArgClause(Lit.String(in.stripSuffix("pisc") + "json") :: Nil))
                     else \("None")
                    )
          ) ::
          ( prog.tail.tail.head match
              case (`(*)`(_, λ(parameters: Term)), _) =>
                Defn.Val(Nil, Pat.Var("π-parameters") :: Nil, None, parameters)
          ) ::
          ( prog.tail.tail.tail.head match
              case (`(*)`(_, λ(traces: (Lit.Null | Term))), _) =>
                Term.Assign(\("π-traces"), traces)
          ) ::
          Program.Main()(prog.drop(1+3))

        val is = prog_.drop(1+3).map(_._2).zipWithIndex.map(_.swap).toMap

        val ls = bind.drop(1+3).filter(_._1.isLeft).map(_.left.get -> _)

        val code = ps.take(1+2).mkString("\n\n") + "\n\n"
                 + (ps.drop(1+2).zipWithIndex.map(_ -> is(_)) ++ ls.map(_.parse[Stat].get -> _))
                   .sortBy(_._2)
                   .map(_._1)
                   .mkString("\n\n")

        val trick = `trick-or-treat`("π-trick", discarded).toString
        val spell = `spell, magic spell`("π-spell", enabled).toString
        val wand = `magic wand`.toString

        val magic = trick + "\n\n" + spell + "\n\n" + wand + "\n\n"
        val elvis = `if-then-else`("π-elvis", excluded).toString + "\n\n"

        val init = Π(
          prog
            .find {
              case (`(*)`("Main"), _) => true
              case _ => false
            }
            .get
            ._2
            .enabled
        ).toString + "\n\n"

        bwr.write(magic + elvis + init + code, 0, magic.length + elvis.length + init.length + code.length)

        if J
        then
          try
            bwr.close()
            fwr.close()
          catch _.printStackTrace
          finally
            fwr = null
            bwr = null
          try
            val ast = Paths.get(s"$examples/", in.stripSuffix("pisc") + "json").toString
            fwr = FileWriter(ast, UTF_8)
            bwr = BufferedWriter(fwr)
            val json = prog.drop(1+3).asJson
            val file = json.spaces2

            bwr.write(file, 0, file.length)

            prog.tail.head match
              case (`(*)`(_, λ(it: Elastic4s)), _) =>
                elastic4s(it, Document.build(prog.drop(1+3), json))

          catch _.printStackTrace
          finally
            try
              if bwr ne null then bwr.close()
              if fwr ne null then fwr.close()
            catch _.printStackTrace
            finally
              fwr = null
              bwr = null

      catch t =>
        spi.last match
          case Some(((it, isMacroExpansion), (line, col))) =>
            Console.err.println(s"Error in file `$in' ${spi.ln}! " + t.getMessage
                              + s""".\nLast known parse${if isMacroExpansion then " (from macro expansion) " else " "}at line #$line, column #$col.\n$it""")
          case _ =>
            Console.err.println(s"Error in file `$in' ${spi.ln}! " + t.getMessage + ".")
        throw t
      finally
        if bwr ne null then bwr.close()
        if fwr ne null then fwr.close()
        if source ne null then source.close()

    args.foreach {
      case "-A" => A = host"localhost"
      case "-P" => P = Int.MaxValue
      case "-H" => H = 0
      case "-T" => T = 123456
      case "-E" => E = true
      case "-I" => I = Nil
      case "-J" => J = false
      case it if it.startsWith("-A") => A = IpAddress.fromString(it.substring(2))
                                                     .orElse(Hostname.fromString(it.substring(2)))
                                                     .getOrElse(host"localhost")
      case it if it.startsWith("-P") => P = it.substring(2).toInt
      case it if it.startsWith("-H") => H = it.substring(2).toInt
      case it if it.startsWith("-T") => T = it.substring(2).toInt
      case it if it.startsWith("-E") => E = it.substring(2).toBoolean
      case it if it.startsWith("-I") => I :::= it.substring(2).split(",").toList.map(Plugin.valueOf)
      case it if it.startsWith("-J") => J = it.substring(2).toBoolean
      case it => bain(it)
    }


  object elastic4s extends ((Elastic4s, Document) => Unit):

    import java.net.http.HttpClient
    import java.security.cert.X509Certificate
    import java.security.SecureRandom
    import javax.net.ssl.{ SSLContext, TrustManager, X509TrustManager }

    import scala.concurrent.duration.Duration
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future

    import com.sksamuel.elastic4s.{ Authentication, CommonRequestOptions, ElasticClient, ElasticNodeEndpoint }
    import com.sksamuel.elastic4s.circe.*
    import com.sksamuel.elastic4s.ElasticDsl.*
    import com.sksamuel.elastic4s.sttp4.SttpRequestHttpClient

    import sttp.monad.{ FutureMonad, MonadError }
    import sttp.client4.Backend
    import sttp.client4.httpclient.HttpClientFutureBackend

    // Create a trust manager that does not validate certificate chains
    val trustAllCerts = Array[TrustManager](new X509TrustManager {
      def getAcceptedIssuers: Array[X509Certificate] = null
      def checkClientTrusted(certs: Array[X509Certificate], authType: String): Unit = {}
      def checkServerTrusted(certs: Array[X509Certificate], authType: String): Unit = {}
    })

    val sslContext = SSLContext.getInstance("TLS")
    sslContext.init(null, trustAllCerts, new SecureRandom())

    val httpClient = HttpClient.newBuilder()
      .sslContext(sslContext)
      .build()

    val index = s"stochasticpicalculus_$examples"

    given Backend[Future] = HttpClientFutureBackend.usingClient(httpClient)

    given MonadError[Future] = new FutureMonad()

    override def apply(it: Elastic4s, doc: Document): Unit = it match {
      case Elastic4s("https", host, port, Some(username), Some(password)) =>
        ElasticClient(new SttpRequestHttpClient(ElasticNodeEndpoint("https", host, port, None)))
     -> CommonRequestOptions.defaults.copy(authentication = Authentication.UsernamePassword(username, password))
      case Elastic4s(_, host, port, _, _) =>
        ElasticClient(new SttpRequestHttpClient(ElasticNodeEndpoint("http", host, port, None)))
     -> CommonRequestOptions.defaults
    } match
        case (client, given CommonRequestOptions) =>
          given Duration = Duration.Inf

          val exists = client.execute(indexExists(index)).await

          if exists.isSuccess && (exists.result.isExists || client.execute(createIndex(index).mapping(properties(Document.schema))).await.isSuccess)
          then
            val into = client.execute(indexInto(index).source(doc)).await
            if into.isSuccess
            then
              println(s"GET /$index/_source/${into.result.id}")
