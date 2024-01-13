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

import _root_.cats.Monad
import _root_.cats.effect.{ IO, Deferred, Ref }
import _root_.cats.effect.std.{ CyclicBarrier, Queue, Semaphore }

import `Π-stats`._


package object `Π-loop`:

  type % = Ref[IO, Map[String, Option[Rate]]]

  type \ = Ref[IO, Set[String]]

  type * = Semaphore[IO]

  type + = CyclicBarrier[IO]

  type - = Ref[IO, Deferred[IO, (String, BigDecimal)]]

  type / = Queue[IO, (String, Rate)]

  def loop(using % : %, \ : \, * : (*, *), + : +, - : -): IO[Unit] =
    for
      it <- %.modify { m =>
                       if m.exists(_._2 ne None)
                       then m -> Some(Map.from(m))
                       else m -> None
            }
      _  <- if it.isEmpty then *._1.acquire else
            for
              (key, delta) <- IO.pure(|(it.get))
              turn         <- -.get
              _            <- turn.complete(key -> delta)
              _            <- *._2.acquire
              _            <- Monad[IO].whileM_(\.modify { d => d -> d.nonEmpty })(IO.cede)
              _            <- %.update(_ - key)
              turn         <- Deferred[IO, (String, BigDecimal)]
              _            <- -.set(turn)
              _            <- *._2.tryAcquire
              _            <- +.await
            yield
              ()
      _  <- IO.cede >> loop
    yield
      ()

  def poll(using % : %, / : /, * : *): IO[Unit] =
    for
      (key, r) <- /.take
      _        <- %.update(_ + (key -> Some(r)))
      _        <- *.release
      _        <- IO.cede >> poll
    yield
      ()
