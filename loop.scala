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

import _root_.cats.effect.{ IO, Deferred, Ref }
import _root_.cats.effect.std.{ CyclicBarrier, Queue, Semaphore }

import `Π-stats`._


package object `Π-loop`:

  import sΠ.{ `Π-Map`, `Π-Set`, >*< }

  type - = CyclicBarrier[IO]

  type + = (Deferred[IO, Option[(Double, -)]], (>*<, Option[Boolean], Rate))

  type % = Ref[IO, Map[String, Int | +]]

  type * = Semaphore[IO]

  type / = Queue[IO, ((String, String), +)]

  def loop(`π-trick`: `Π-Map`[String, `Π-Set`[String]])
          (using % : %, * : *): IO[Unit] =
    for
      it <- %.modify { m =>
                       if m.exists(_._2.isInstanceOf[Int])
                       then m -> Map.empty
                       else m -> m
                         .map(_ -> _.asInstanceOf[+])
                         .map(_ -> _._2)
                         .toMap
            }
      _  <- if it.isEmpty
            then *.acquire                   // ,- parallelism
            else                             // |
              for                            // v
                opt <- IO.pure(|(it)(`π-trick`)(6))
                _   <- if opt.isEmpty
                       then IO.cede >> loop(`π-trick`)
                       else
                         for
                           nel <- IO.pure(opt.get)
                           ios  = nel.map { case (key1, key2, delay) =>
                                            for
                                              -         <- CyclicBarrier[IO](if key1 == key2 then 2 else 3)
                                              deferred1 <- %.modify { m => m -> m(key1).asInstanceOf[+]._1 }
                                              deferred2 <- %.modify { m => m -> m(key2).asInstanceOf[+]._1 }
                                              _         <- %.update(_ - key1 - key2)
                                              _         <- deferred1.complete(Some(delay -> -))
                                              _         <- deferred2.complete(Some(delay -> -))
                                              _         <- -.await
                                            yield
                                              ()
                                          }
                           _   <- ios.parSequence.void
                         yield
                           ()
              yield
                ()
      _  <- IO.cede >> loop(`π-trick`)
    yield
      ()

  def poll(using % : %, / : /, * : *): IO[Unit] =
    for
      h <- /.take
      ((_, key), it) = h
      _ <- %.update { m =>
                      val ^ = h._1._1
                      ( if m(key) == 1
                        then
                          m - key
                        else
                          m + (key -> (m(key).asInstanceOf[Int] - 1))
                      ) + (^ + key -> it)
           }
      _ <- *.release
      _ <- IO.cede >> poll
    yield
      ()
