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

import _root_.scala.collection.immutable.List
import _root_.scala.Option.unless

import _root_.cats.instances.list.*
import _root_.cats.syntax.applicative.*
import _root_.cats.syntax.functor.*
import _root_.cats.syntax.flatMap.*
import _root_.cats.syntax.traverse.*

import _root_.cats.effect.{ Async, ExitCode }
import _root_.cats.effect.std.Queue

import `Π-loop`.*
import `Π-traces`.*


package object `Π-dump`:

  private val spirsx = "pisc.stochastic.replications.exitcode.ignore"


  type -[F[_]] = Queue[F, Option[((Long, Double), ((Long, Long), Long), (String, String), (Double, Double))]]


  final class πdump[F[_]: Async]:

    private def record(number: Long, clock: Double, started: Long, ended: Long, delay: Double, duration: Double): String => F[Unit] =
      _.split(",") match
        case Array(key, name, polarity, label, rate, agent) =>
          Async[F].blocking {
            `π-traces`(number, clock, started, ended,
                       agent, name, unless(polarity.isEmpty)(java.lang.Boolean.parseBoolean(polarity)),
                       key.stripPrefix("!"), key.startsWith("!"), label,
                       rate, delay, duration)
          }
        case _ =>
          Async[F].unit

    private def doExit(using % : %[F], ! : ![F]): F[Unit] =
      %.get.flatMap { m =>
        val ks = m.keys.toList
        val ec =
          if ks.isEmpty
          then
            ExitCode.Success
          else
            if !sys.BooleanProp.keyExists(spirsx).value
            && ks.forall(_.charAt(36) == '!')
            then ExitCode.Success
            else ExitCode.Error
        ks.traverse(m(_).asInstanceOf[(Boolean, +[F])]._2._1._1._1.complete(None)) >>
        ks.traverse(m(_).asInstanceOf[(Boolean, +[F])]._2._1._1._2 match { case null => Async[F].unit
                                                                           case it => it.get.flatMap(_.complete(None).void) }) >>
        !.complete(ec).void
      }

    def dump(using % : %[F], ! : ![F], - : -[F]): F[Unit] =
      for
        h <- -.take
        _ <- h match
               case Some(_) if `π-traces` eq null =>
                 dump
               case Some(((no, cl), ((s1, s2), e), (k1, k2), (delay, duration))) =>
                 for
                   _ <- record(no, cl, s1, e, delay, duration)(k1)
                   _ <- record(no, cl, s2, e, delay, duration)(k2).unlessA(k1 == k2)
                   _ <- Async[F].cede >> dump
                 yield
                   ()
               case _ =>
                 Async[F].blocking(`π-traces`.close).whenA(`π-traces` ne null) >> doExit
      yield
        ()
