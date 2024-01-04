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

package object `Π`:

  import cats.effect.{ Deferred, Ref, IO }

  import `Π-magic`._


  /**
    * restriction aka new name
    */
  object `v`:

    def map(f: `()` => Unit): IO[Unit] = flatMap(f andThen IO.pure)
    def flatMap(f: `()` => IO[Unit]): IO[Unit] =
      ( for
          ref <- Ref.of[IO, `><`](`><`())
        yield
          f(`()`(ref))
      ).flatten


  /**
    * silent transition
    */
  val `𝜏` = IO.unit


  /**
    * prefix
    */
  final implicit class `()`(val name: Any) extends AnyVal:

    private def ref = name.asInstanceOf[Ref[IO, `><`]]

    inline def ===(that: `()`) = this.name == that.name

    /**
      * positive prefix i.e. input
      */
    def apply(): IO[`()`] = `><`()(ref).map(`()`)

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`): IO[Unit] = `><`(value.name)(ref)

    override def toString: String = name.toString


  object `Π-magic`:

    /**
      * Adapted from cats-effect tutorial [[https://typelevel.org/cats-effect/docs/tutorial]].
      *
      * @see [[https://github.com/lrodero/cats-effect-tutorial/blob/series/3.x/src/main/scala/catseffecttutorial/producerconsumer/ProducerConsumerBoundedCancelable.scala]]
      */
    /*
     *
     * Copyright (c) 2020 Luis Rodero-Merino
     *
     * Licensed under the Apache License, Version 2.0 (the "License");
     * you may not use this file except in compliance with the License.
     * You may obtain a copy of the License at.
     *
     *     http://www.apache.org/licenses/LICENSE-2.0
     *
     * Unless required by applicable law or agreed to in writing, software
     * distributed under the License is distributed on an "AS IS" BASIS,
     * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
     * See the License for the specific language governing permissions and
     * limitations under the License.
     */

    final case class `><`(takers: List[Deferred[IO, Any]], offerers: List[(Any, Deferred[IO, Unit])])

    object `><`:

      inline def apply(): `><` = `><`(Nil, Nil)

      import scala.util.Random

      val random = Random()

      def apply(name: Any)(`>R`: Ref[IO, `><`]): IO[Unit] =
          Deferred[IO, Unit].flatMap { offerer =>
            IO.uncancelable { poll => // `poll` used to embed cancelable code, i.e. the call to `offerer.get`
              `>R`.modify {
                case `><`(takers, offerers) if takers.nonEmpty =>
                  val i = math.abs(random.nextInt) % takers.size
                  val (taker, rest) = takers(i) -> (takers.take(i) ++ takers.drop(i+1))
                  `><`(rest, offerers) -> taker.complete(name).void
                case `><`(takers, offerers) =>
                  val cleanup = `>R`.update { it => it.copy(offerers = it.offerers.filter(_._2 ne offerer)) }
                  `><`(takers, name -> offerer :: offerers) -> poll(offerer.get).onCancel(cleanup)
              }.flatten
            }
          }

      def apply()(`<R`: Ref[IO, `><`]): IO[Any] =
          Deferred[IO, Any].flatMap { taker =>
            IO.uncancelable { poll =>
              `<R`.modify {
                case `><`(takers, offerers) if offerers.nonEmpty =>
                  val i = math.abs(random.nextInt) % offerers.size
                  val ((name, release), rest) = offerers(i) -> (offerers.take(i) ++ offerers.drop(i+1))
                  `><`(takers, rest) -> release.complete(()).as(name)
                case `><`(takers, offerers) =>
                  val cleanup = `<R`.update { it => it.copy(takers = it.takers.filter(_ ne taker)) }
                  `><`(taker :: takers, offerers) -> poll(taker.get).onCancel(cleanup)
              }.flatten
            }
          }
