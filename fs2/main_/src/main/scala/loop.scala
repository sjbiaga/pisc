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

import _root_.scala.collection.immutable.{ List, Map, Set }
import _root_.scala.concurrent.duration.*
import _root_.scala.Option.{ unless, when }

import _root_.cats.instances.list.*
import _root_.cats.syntax.applicative.*
import _root_.cats.syntax.applicativeError.*
import _root_.cats.syntax.functor.*
import _root_.cats.syntax.flatMap.*
import _root_.cats.syntax.parallel.*
import _root_.cats.syntax.semigroupal.*
import _root_.cats.syntax.traverse.*

import _root_.cats.Parallel
import _root_.cats.effect.{ Deferred, Fiber, ExitCode, Ref, Resource, Temporal }
import _root_.cats.effect.kernel.Outcome.Succeeded
import _root_.cats.effect.std.{ AtomicCell, CyclicBarrier, Queue, Semaphore, UUIDGen }
import _root_.cats.effect.syntax.monadCancel.*
import _root_.cats.effect.syntax.spawn.*
import _root_.cats.effect.syntax.temporal.*

import `Π-dump`.*
import `Π-stats`.*
import `Π-traces`.{ KeyBy, Plugin }


package object `Π-loop`:

  import sΠ.{ `Π-Map`, `Π-Set`, `{}`, `[]`, `()` }


  type <>[F[_]] = (CyclicBarrier[F], Fiber[F, Throwable, Unit], Ref[F, `()`[F]], `[]`)

  type ++[F[_]] = ((Deferred[F, Option[<>[F]]], Ref[F, Deferred[F, Option[<>[F]]]]), Ref[F, Long])
  type +[F[_]] = (++[F], (`{}`[F], Option[Either[Unit, Ref[F, `()`[F]]]], Rate, `[]`))

  type %[F[_]] = AtomicCell[F, Map[String, Int | (Boolean, +[F])]]

  type !![F[_]] = Deferred[F, Unit]
  type ![F[_]] = Deferred[F, ExitCode]

  type &|[F[_]] = Ref[F, Long]

  type /[F[_]] = Queue[F, ((String, String), +[F])]

  type \[F[_]] = F[Unit] => F[Unit]

  type ++++[F[_]] = ((Double, Seq[Plugin]), (`{}`[F], Ref[F, `()`[F]]), (++[F], ++[F]))
  type **[F[_]] = Queue[F, List[((String, String), ++++[F])]]

  type *[F[_]] = Semaphore[F]

  type ^[F[_]] = Resource[F, Unit]


  final case class `Π-Parameters`(address: String,
                                  parallelism: Int,
                                  threshold: Int,
                                  timeout: Int,
                                  exit: Boolean,
                                  plugins: Set[String],
                                  causal: Boolean = false)

  final case class Feedback[F[_]](paramsRD: Ref[F, Deferred[F, `Π-Parameters`]],
                                  paramsR: Ref[F, `Π-Parameters`],
                                  tracesR: Ref[F, Boolean],
                                  keyByR: Ref[F, Boolean],
                                  lastR: Ref[F, (Long, Double)],
                                  pauseRD_stopR_exitRD: AtomicCell[F, ((Deferred[F, Unit], Boolean), Deferred[F, Unit])],
                                  initR: Ref[F, Boolean],
                                  doneR: Ref[F, Boolean])


  def `π-enable`[F[_]](enabled: `Π-Set`[String])
                      (using % : %[F]): F[Unit] =
    %.update(enabled.foldLeft(_) { (m, key) =>
                                    val n = if m.contains(key)
                                            then m(key).asInstanceOf[Int]
                                            else 0
                                    m + (key -> (n + 1))
                                 }
    )

  final class πloop[F[_]: Temporal: Parallel: UUIDGen]:

    private def enable(key: String)
                      (using %[F])
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): F[Unit] =
      val (_, spell) = `π-wand`
      `π-enable`(spell(key))


    private def unblock(map: Map[String, Int | (Boolean, +[F])], key: String)
                       (implicit ^ : String): F[Unit] =
      map(^ + key).asInstanceOf[(Boolean, +[F])]._2._1._1._1.complete(None).void.whenA(map.contains(^ + key))

    private def `π-discard`(map: Map[String, Int | (Boolean, +[F])], discarded: `Π-Set`[String])
                           (implicit ^ : String): F[Set[String]] =
      discarded.toList.traverse(unblock(map, _)).as(discarded.map(^ + _))

    private def discard(key: String, map: Map[String, Int | (Boolean, +[F])])
                       (using String)
                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): F[Set[String]] =
      val (trick, _) = `π-wand`
      if trick.contains(key)
      then
        `π-discard`(map, trick(key))
      else
        Temporal[F].pure(Set.empty)


    def peek(plugins: Set[String])
            (using % : %[F], ** : **[F])
            (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): F[Unit] =
      %.evalModify { m =>
        val it =
          if m.exists(_._2.isInstanceOf[Int])
          then Map.empty
          else m
               .filter(_._2.asInstanceOf[(Boolean, +[F])]._1)
               .map(_ -> _.asInstanceOf[(Boolean, +[F])]._2._2)
               .toMap
        if it.isEmpty
        then
          Temporal[F].pure(m -> ())
        else
          ∥(it, plugins)(`π-wand`._1)().map {
            case (key1, key2, ein, dp) =>
              val (dcts1, _) = m(key1).asInstanceOf[(Boolean, +[F])]._2
              val (dcts2, _) = m(key2).asInstanceOf[(Boolean, +[F])]._2
              (key1, key2) -> (dp, ein, (dcts1, dcts2))
          } match
            case Nil =>
              Temporal[F].pure(m -> ())
            case nel =>
              nel.traverse {
                case ((key1, key2), _) =>
                  val k1 = key1.substring(36)
                  val k2 = key2.substring(36)
                  val  ^ = key1.substring(0, 36)
                  val ^^ = key2.substring(0, 36)
                  val (((d1, c1), _), _) = m(key1).asInstanceOf[(Boolean, +[F])]._2
                  val (((d2, c2), _), _) = m(key2).asInstanceOf[(Boolean, +[F])]._2
                  for
                    s1 <- d1.tryGet.map(_ eq None).flatMap { if _ then discard(k1, m)(using  ^) else Temporal[F].pure(Set.empty) }
                    s2 <- if k1 == k2
                          then Temporal[F].pure(Set.empty)
                          else d2.tryGet.map(_ eq None).flatMap { if _ then discard(k2, m)(using ^^) else Temporal[F].pure(Set.empty) }
                  yield
                    (s1 ++ s2 ++ when(c1 eq null)(key1) ++ when(c2 eq null)(key2))
                 -> (Nil ++ unless(c1 eq null)(key1) ++ unless(k1 == k2)(unless(c2 eq null)(key2)).flatten)
              }.map(_.foldRight(m) {
                      case ((ks, ls), map) =>
                        ls.map { key => key -> (false, map(key).asInstanceOf[(Boolean, +[F])]._2) }
                          .foldLeft(ks.foldLeft(map)(_ - _))(_ + _)
                    }
              ).flatMap(mʹ => **.offer(nel).map(mʹ -> _))
      }


    private def canExit(using % : %[F])
                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): F[Boolean] =
      def exit(m: Map[String, Int | (Boolean, +[F])]): Boolean =
        val (trick, _) = `π-wand`
        !m.exists(_._2.isInstanceOf[Int])
        && m.forall {
             case (_, (true, (_, (_, None, _, _)))) => false
             case (key1, (true, (_, (e1, Some(p1), _, _)))) =>
               val ^ = key1.substring(0, 36)
               !m.exists {
                 case (key2, (true, (_, (e2, Some(p2), _, _)))) if (e1 eq e2) && p1.isLeft == p2.isRight =>
                   val ^^ = key2.substring(0, 36)
                   ^ != ^^
                   || {
                     val k1 = key1.substring(36)
                     val k2 = key2.substring(36)
                     !trick.contains(k1) || !trick(k1).contains(k2)
                   }
                 case _ => false
               }
             case _ => true
           }
      %.modify { m => m -> exit(m) }

    private def doExit(feedback: Feedback[F])
                      (using % : %[F]): F[Unit] =
      feedback.doneR.set(true) >> feedback.pauseRD_stopR_exitRD.get.flatMap(_._2.get) >> %.get.flatMap { m =>
        val ks = m.filterNot(_._2.isInstanceOf[Int]).keys.toList
        ks.traverse(m(_).asInstanceOf[(Boolean, +[F])]._2._1._1._1.complete(None)) >>
        ks.traverse(m(_).asInstanceOf[(Boolean, +[F])]._2._1._1._2 match { case null => Temporal[F].unit
                                                                           case it => it.get.flatMap(_.complete(None).void) }).void
      }


    def loopʹ(parameters: `Π-Parameters`, started: Ref[F, Long], batch: Ref[F, Long], feedback: Feedback[F])
             (using % : %[F], &| : &|[F], - : -[F], * : *[F], ** : **[F], ^ : ^[F])
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): F[Unit] =
      for
        _ <- batch.set(0L) >> *.acquire.guaranteeCase { case Succeeded(_) => batch.update(_ + 1) case _ => Temporal[F].unit }.replicateA_(parameters.threshold).timeoutTo(parameters.timeout.microseconds, Temporal[F].unit)
        m  =
          for
            nel <- **.tryTake.map(_.getOrElse(Nil))
            stp <-
              if nel.isEmpty
              then
                (started.get product batch.get).map(_ + _).flatMap {
                  case 0L =>
                    canExit.ifM(doExit(feedback) >> -.offer(None) >> Temporal[F].pure(None), Temporal[F].pure(Some(false)))
                  case _  =>
                    feedback.pauseRD_stopR_exitRD.get.map(_._1._2).ifM(doExit(feedback) >> -.offer(None) >> Temporal[F].pure(Some(true)), Temporal[F].pure(Some(false)))
                }
              else
                (feedback.pauseRD_stopR_exitRD.get.map(_._1._2) product Semaphore[F](parameters.parallelism)).flatMap { (stop, sem) =>
                  &|.get.flatMap { i =>
                    val fun = { (f: ((((String, String), ++++[F]), Int)) => F[Unit]) => if parameters.parallelism == 1 then nel.zipWithIndex.traverse(f) else nel.zipWithIndex.parTraverse(f) }
                    fun { case (((key1, key2), (dp @ (_, plugins), (eth, in), (((d1, c1), ts1), ((d2, c2), ts2)))), j) =>
                            val k1 = key1.substring(36)
                            val k2 = key2.substring(36)
                            if stop
                            then
                              for
                                _  <- d1.complete(None)
                                _  <- d2.complete(None).unlessA(k1 == k2)
                                _  <- c1.get.flatMap(_.complete(None)).unlessA(c1 eq null)
                                _  <- c2.get.flatMap(_.complete(None)).unlessA(c2 eq null).unlessA(k1 == k2)
                              yield
                                ()
                            else
                              for
                                cb <- CyclicBarrier[F](if k1 == k2 then 2 else 3)
                                no <- &|.updateAndGet(_ + 1)
                                _  <- sem.acquire
                                _  <- eth.fold(Temporal[F].unit)(_.acquire)
                                _  <- started.update(_ + 1)
                                fb <- ( for
                                          _  <- cb.await
                                          _  <- enable(k1)
                                          _  <- enable(k2).unlessA(k1 == k2)
                                          ss <- ts1.get product ts2.get
                                          kb <- if parameters.causal then Temporal[F].pure(KeyBy.HID)
                                                else feedback.keyByR.get.map(if _ then KeyBy.AGENT_LABEL else KeyBy.ANY)
                                          now <- Temporal[F].realTime.map(_.toMillis)
                                          _  <- feedback.tracesR.get >>= -.offer(Some((no, (ss, now), (k1, k2, kb), (i -> j, dp)))).whenA
                                          _  <- eth.fold(Temporal[F].unit)(_.release)
                                          _  <- sem.release
                                          _  <- started.update(_ - 1)
                                        yield
                                          ()
                                        ).start
                                cs  = plugins.find(_.isInstanceOf[Plugin.causes]) match
                                        case Some(Plugin.causes(cs)) =>
                                          if k1.indexOf(',') < 0 || k1 != k2 && k2.indexOf(',') < 0
                                          then
                                            cs
                                          else
                                            cs + no
                                        case _ =>
                                          Set.empty
                                _  <- d1.complete(Some((cb, fb, in, cs)))
                                _  <- d2.complete(Some((cb, fb, in, cs))).unlessA(k1 == k2)
                                _  <- c1.get.flatMap(_.complete(Some((cb, fb, in, cs)))).unlessA(c1 eq null)
                                _  <- c2.get.flatMap(_.complete(Some((cb, fb, in, cs)))).unlessA(c2 eq null).unlessA(k1 == k2)
                              yield
                                ()
                        }
                  }
                } >> Temporal[F].pure(Some(false))
          yield
            stp
        s <- ^.use(_ => (*.available >>= *.acquireN) >> peek(parameters.plugins) >> m)
        _ <- s.fold(Temporal[F].never)(if _ then stopʹ(parameters)
                                       else feedback.pauseRD_stopR_exitRD.get.flatMap(_._1._1.get)
                                         >> feedback.paramsRD.get.flatMap(_.tryGet).flatMap {
                                              case Some(params) =>
                                                feedback.paramsR.set(params) >>
                                                Temporal[F].deferred[`Π-Parameters`].flatMap(feedback.paramsRD.set) >>
                                                Temporal[F].cede >> loopʹ(params, started, batch, feedback)
                                              case _ =>
                                                Temporal[F].cede >> loopʹ(parameters, started, batch, feedback)
                                            })
      yield
        ()

    private def stopʹ(parameters: `Π-Parameters`)
                     (using % : %[F], * : *[F], ** : **[F], ^ : ^[F])
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): F[Unit] =
      for
        _ <- *.acquireN(parameters.threshold).timeoutTo(parameters.timeout.microseconds, Temporal[F].unit)
        m  =
          for
            nel  <- **.tryTake.map(_.getOrElse(Nil))
            _    <- nel.traverse {
                      case ((key1, key2), (_, _, (((d1, c1), _), ((d2, c2), _)))) =>
                        val k1 = key1.substring(36)
                        val k2 = key2.substring(36)
                        for
                          _ <- d1.complete(None)
                          _ <- d2.complete(None).unlessA(k1 == k2)
                          _ <- c1.get.flatMap(_.complete(None)).unlessA(c1 eq null)
                          _ <- c2.get.flatMap(_.complete(None)).unlessA(c2 eq null).unlessA(k1 == k2)
                        yield
                          ()
                    }
          yield
            ()
        _ <- ^.use(_ => (*.available >>= *.acquireN) >> peek(parameters.plugins) >> m)
        _ <- Temporal[F].cede >> stopʹ(parameters)
      yield
        ()

    def loop0(parameters: `Π-Parameters`, started: Ref[F, Long], feedback: Feedback[F])
             (using % : %[F], &| : &|[F], - : -[F], * : *[F], ** : **[F])
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): F[Unit] =
      for
        nel  <- **.take
        opt  <- ( if nel.isEmpty
                  then
                    (started.get product **.size.map(_.toLong)).map(_ + _).flatMap {
                      case 0L =>
                        **.take.map(Some(_)).timeoutTo(parameters.timeout.microseconds, Temporal[F].pure(None))
                      case _  =>
                        Temporal[F].pure(Some(nel))
                    }
                  else
                    Temporal[F].pure(Some(nel))
                )
        _    <- ( if opt eq None
                  then
                    canExit.ifM(doExit(feedback) >> -.offer(None) >> Temporal[F].never, Temporal[F].unit)
                  else if opt.get.isEmpty
                  then
                    feedback.pauseRD_stopR_exitRD.get.map(_._1._2).ifM(doExit(feedback) >> -.offer(None) >> stop0, Temporal[F].unit)
                  else
                    val nel = opt.get
                    (feedback.pauseRD_stopR_exitRD.get.map(_._1._2) product Semaphore[F](parameters.parallelism)).flatMap { (stop, sem) =>
                      &|.get.flatMap { i =>
                        val fun = { (f: ((((String, String), ++++[F]), Int)) => F[Unit]) => if parameters.parallelism == 1 then nel.zipWithIndex.traverse(f) else nel.zipWithIndex.parTraverse(f) }
                        fun { case (((key1, key2), (dp @ (_, plugins), (eth, in), (((d1, c1), ts1), ((d2, c2), ts2)))), j) =>
                                val k1 = key1.substring(36)
                                val k2 = key2.substring(36)
                                if stop
                                then
                                  for
                                    _ <- **.offer(Nil)
                                    _  <- d1.complete(None)
                                    _  <- d2.complete(None).unlessA(k1 == k2)
                                    _  <- c1.get.flatMap(_.complete(None)).unlessA(c1 eq null)
                                    _  <- c2.get.flatMap(_.complete(None)).unlessA(c2 eq null).unlessA(k1 == k2)
                                  yield
                                    ()
                                else
                                  for
                                    cb <- CyclicBarrier[F](if k1 == k2 then 2 else 3)
                                    no <- &|.updateAndGet(_ + 1)
                                    _  <- sem.acquire
                                    _  <- eth.fold(Temporal[F].unit)(_.acquire)
                                    _  <- started.update(_ + 1)
                                    fb <- ( for
                                              _  <- cb.await
                                              _  <- enable(k1)
                                              _  <- enable(k2).unlessA(k1 == k2)
                                              ss <- ts1.get product ts2.get
                                              kb <- if parameters.causal then Temporal[F].pure(KeyBy.HID)
                                                    else feedback.keyByR.get.map(if _ then KeyBy.AGENT_LABEL else KeyBy.ANY)
                                              now <- Temporal[F].realTime.map(_.toMillis)
                                              _  <- feedback.tracesR.get >>= -.offer(Some((no, (ss, now), (k1, k2, kb), (i -> j, dp)))).whenA
                                              _  <- eth.fold(Temporal[F].unit)(_.release)
                                              _  <- sem.release
                                              _  <- started.updateAndGet(_ - 1).map(_ == 0) >>= **.offer(Nil).whenA
                                            yield
                                              ()
                                          ).start
                                    cs  = plugins.find(_.isInstanceOf[Plugin.causes]) match
                                            case Some(Plugin.causes(cs)) =>
                                              if k1.indexOf(',') < 0 || k1 != k2 && k2.indexOf(',') < 0
                                              then
                                                cs
                                              else
                                                cs + no
                                            case _ =>
                                              Set.empty
                                    _  <- d1.complete(Some((cb, fb, in, cs)))
                                    _  <- d2.complete(Some((cb, fb, in, cs))).unlessA(k1 == k2)
                                    _  <- c1.get.flatMap(_.complete(Some((cb, fb, in, cs)))).unlessA(c1 eq null)
                                    _  <- c2.get.flatMap(_.complete(Some((cb, fb, in, cs)))).unlessA(c2 eq null).unlessA(k1 == k2)
                                  yield
                                    ()
                            }
                      }
                    }.void
                )
        _    <- feedback.pauseRD_stopR_exitRD.get.flatMap(_._1._1.get)
        _    <- feedback.paramsRD.get.flatMap(_.tryGet).flatMap {
                  case Some(params) =>
                    feedback.paramsR.set(params) >>
                    Temporal[F].deferred[`Π-Parameters`].flatMap(feedback.paramsRD.set) >>
                    Temporal[F].cede >> loop0(params, started, feedback)
                  case _ =>
                    Temporal[F].cede >> loop0(parameters, started, feedback)
                }
      yield
        ()

    private def stop0(using ** : **[F]): F[Unit] =
      for
        nel  <- **.take
        _    <- nel.traverse {
                  case ((key1, key2), (_, _, (((d1, c1), _), ((d2, c2), _)))) =>
                    val k1 = key1.substring(36)
                    val k2 = key2.substring(36)
                    for
                      _ <- d1.complete(None)
                      _ <- d2.complete(None).unlessA(k1 == k2)
                      _ <- c1.get.flatMap(_.complete(None)).unlessA(c1 eq null)
                      _ <- c2.get.flatMap(_.complete(None)).unlessA(c2 eq null).unlessA(k1 == k2)
                    yield
                      ()
                }
        _    <- Temporal[F].cede >> stop0
      yield
        ()

    def poll(using % : %[F], / : /[F], \ : \[F]): F[Unit] =
      /.take.flatMap {
        case ((^ @ (_: String), key), it @ (((d, _), _), _)) =>
          d.tryGet.map(_ eq None).flatMap {
            if _
            then
              \(
                %.update { m =>
                           val n = m(key).asInstanceOf[Int] - 1
                           ( if n == 0
                             then
                               m - key
                             else
                               m + (key -> n)
                           ) + (^ + key -> (true, it))
                }
              )
            else
              %.update(_ + (^ + key -> (false, it)))
          } >> Temporal[F].cede >> poll
      }
