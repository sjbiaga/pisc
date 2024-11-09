/*
 * Copyright (c) 2023-2024 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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

import _root_.scala.collection.immutable.Map

import _root_.cats.syntax.parallel._

import _root_.cats.data.NonEmptyList
import _root_.cats.effect.{ IO, Deferred, Ref }
import _root_.cats.effect.std.{ CyclicBarrier, Queue, Semaphore }

import `Π-stats`._


package object `Π-loop`:

  import sΠ.{ `Π-Map`, `Π-Set`, >*< }
  export sΠ.`π-exclude`

  type - = CyclicBarrier[IO]

  type + = (Deferred[IO, Option[(Double, (-, -))]], (>*<, Option[Boolean], Rate))

  type % = Ref[IO, Map[String, Int | +]]

  type * = Semaphore[IO]

  type / = Queue[IO, ((String, String), +)]

  type \ = () => IO[Unit]


  def `π-enable`(enabled: `Π-Set`[String])
                (using % : %): IO[Unit] =
    %.update(enabled.foldLeft(_) { (m, key) =>
                                   val n = if m.contains(key)
                                           then m(key).asInstanceOf[Int]
                                           else 0
                                   m + (key -> (n + 1))
                                 }
    )

  private def ready(key: String)
                   (using % : %)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Unit] =
    val (_, spell) = `π-wand`
    ( if spell.contains(key)
      then
        `π-enable`(spell(key))
      else
        IO.unit
    )


  private def unblock(m: Map[String, Int | +], k: String)
                     (implicit ^ : String): IO[Unit] =
    if m.contains(^ + k)
    then m(^ + k).asInstanceOf[+]._1.complete(None).void
    else IO.unit

  private def `π-discard`(discarded: `Π-Set`[String])
                         (using % : %)
                         (implicit ^ : String): IO[Unit] =
    for
      m <- %.get
      _ <- if discarded.isEmpty then IO.unit
           else NonEmptyList.fromList(discarded.toList).get.traverse(unblock(m, _)).void
      _ <- %.update(discarded.map(^ + _).foldLeft(_)(_ - _))
    yield
      ()

  private def discard(key: String, scope: String)
                     (using % : %)
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Unit] =
    val (trick, _) = `π-wand`
    if trick.contains(key)
    then
      implicit val ^ : String = scope
      `π-discard`(trick(key))
    else
      IO.unit


  def loop(using % : %, * : *)
          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Unit] =
    for
      it <- %.modify { m =>
                       if m.exists(_._2.isInstanceOf[Int])
                       then m -> Map.empty
                       else m -> m
                                 .map(_ -> _.asInstanceOf[+])
                                 .map(_ -> _._2)
                                 .toMap
            }
      _  <- if it.isEmpty               // ,- parallelism
            then *.acquire              // |
            else                        // v
              val opt = |(it)(`π-wand`._1)(9)
              if opt.isEmpty
              then
                val nel = NonEmptyList.fromList(it.map(_._1).toList).get
                for
                  _ <- nel.traverse { key =>
                                      for
                                        d <- %.modify { m => m -> m(key).asInstanceOf[+]._1 }
                                        _ <- d.complete(None)
                                      yield
                                        ()
                                    }
                yield
                  ()
              else
                val nel = opt.get
                for
                  _ <- nel.parTraverse { case (key1, key2, delay) =>
                                         val k1 = key1.substring(36)
                                         val k2 = key2.substring(36)
                                         val ^  = key1.substring(0, 36)
                                         val ^^ = key2.substring(0, 36)
                                         for
                                           -  <- CyclicBarrier[IO](if k1 == k2 then 2 else 3)
                                           -- <- CyclicBarrier[IO](if k1 == k2 then 2 else 3)
                                           t1 <- %.modify { m => m -> m(key1).asInstanceOf[+] }
                                           d2 <- %.modify { m => m -> m(key2).asInstanceOf[+]._1 }
                                           (d1, (ref, _, _)) = t1
                                           _  <- discard(k1, ^)
                                           _  <- if k1 == k2 then IO.unit else discard(k2, ^^)
                                           _  <- %.update(_ - key1 - key2)
                                           _  <- d1.complete(Some(delay -> (-, --)))
                                           _  <- if k1 == k2 then IO.unit else d2.complete(Some(delay -> (-, --)))
                                           _  <- -.await
                                           st <- ref.modify { it => it -> it.stop }
                                           _  <- if st then IO.unit else ready(k1)
                                           _  <- if k1 == k2 then IO.unit else ready(k2)
                                           _  <- --.await
                                         yield
                                           ()
                                       }
                yield
                  ()
      _  <- IO.cede >> loop
    yield
      ()

  def poll(using % : %, / : /, * : *): IO[Unit] =
    for
      h <- /.take
      ((_, key), it) = h
      _ <- %.update { m =>
                      val ^ = h._1._1
                      val n = m(key).asInstanceOf[Int] - 1
                      ( if n == 0
                        then
                          m - key
                        else
                          m + (key -> n)
                      ) + (^ + key -> it)
           }
      _ <- *.release
      _ <- IO.cede >> poll
    yield
      ()
