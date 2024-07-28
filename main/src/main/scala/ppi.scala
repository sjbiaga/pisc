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

package object Π:

  import _root_.scala.collection.immutable.List

  import _root_.cats.effect.{ Deferred, Ref, IO }
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.Supervisor

  import `Π-magic`._


  /**
    * Supervised [[code]].
    * @param code
    */
  private def supervised[T](code: IO[T]): IO[T] =
    ( for
        fiber <- Supervisor[IO](await = true).use(_.supervise(code))
        Succeeded(it) <- fiber.join
      yield
        it
    ).flatten


  /**
    * restriction aka new name
    */
  object ν:

    def map(f: `()` => Unit): IO[Unit] = flatMap(f andThen IO.pure)
    def flatMap(f: `()` => IO[Unit]): IO[Unit] =
      ( for
          ref <- Ref.of[IO, ><](><())
        yield
          f(`()`(ref))
      ).flatten


  /**
    * silent transition
    */
  val τ = IO.unit


  /**
    * prefix
    */
  final implicit class `()`(private val name: Any) extends AnyVal:

    private def ref = name.asInstanceOf[>*<]

    def ====(that: `()`) =
      try
        this.ref eq that.ref
      catch
        case _ =>
          this.name == that.name

    inline def unary_! : Boolean = name == null
    inline def as[T]: T = name.asInstanceOf[T]

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`*): IO[Option[Unit]] = ><(value.map(_.name))(ref)

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`*)(code: => IO[Any]): IO[Option[Unit]] = ><(value.map(_.name))(ref)(code)

    /**
      * positive prefix i.e. input
      */
    def apply(): IO[Seq[`()`]] = ><()(ref).map(_.map(`()`))

    /**
      * positive prefix i.e. input
      */
    def apply()(code: Seq[Any] => IO[Seq[Any]]): IO[Seq[`()`]] = ><()(ref)(code).map(_.map(`()`))

    override def toString: String = if name == null then "null" else name.toString


  private object `Π-magic`:

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

    final case class ><(takers: List[Deferred[IO, Seq[Any]]],
                        offerers: List[(Seq[Any], Deferred[IO, Option[Unit]])])

    type >*< = Ref[IO, ><]

    object >< :

      inline def apply(): >< = ><(Nil, Nil)

      import _root_.scala.util.Random

      private val random = Random()

      def apply(names: Seq[Any])(`>R`: >*<): IO[Option[Unit]] =
        Deferred[IO, Option[Unit]].flatMap { offerer =>
          IO.uncancelable { poll =>
            `>R`.modify {
              case it @ ><(takers, _) if takers.nonEmpty =>
                val i = random.nextInt(takers.size)
                val (taker, rest) = takers(i) -> (takers.take(i) ++ takers.drop(i+1))
                it.copy(takers = rest) -> taker.complete(names).as(Some(()))
              case it =>
                val cleanup = `>R`.update { it => it.copy(offerers = it.offerers.filter(_._2 ne offerer)) }
                it.copy(offerers = names -> offerer :: it.offerers) -> poll(offerer.get).onCancel(cleanup)
            }.flatten
          }
        }

      def apply(names: Seq[Any])(`>R`: >*<)(code: => IO[Any]): IO[Option[Unit]] =
        Deferred[IO, Option[Unit]].flatMap { offerer =>
          IO.uncancelable { poll =>
            `>R`.modify {
              case it @ ><(takers, _) if takers.nonEmpty =>
                val i = random.nextInt(takers.size)
                val (taker, rest) = takers(i) -> (takers.take(i) ++ takers.drop(i+1))
                it.copy(takers = rest) -> taker.complete(names).as(Some(()))
              case it =>
                val cleanup = `>R`.update { it => it.copy(offerers = it.offerers.filter(_._2 ne offerer)) }
                it.copy(offerers = names -> offerer :: it.offerers) -> poll(offerer.get).onCancel(cleanup)
            }.flatten <* supervised(code)
          }
        }

      def apply()(`<R`: >*<): IO[Seq[Any]] =
        Deferred[IO, Seq[Any]].flatMap { taker =>
          IO.uncancelable { poll =>
            `<R`.modify {
              case it @ ><(_, offerers) if offerers.nonEmpty =>
                val i = random.nextInt(offerers.size)
                val ((names, offerer), rest) = offerers(i) -> (offerers.take(i) ++ offerers.drop(i+1))
                it.copy(offerers = rest) -> offerer.complete(Some(())).as(names)
              case it =>
                val cleanup = `<R`.update { it => it.copy(takers = it.takers.filter(_ ne taker)) }
                it.copy(takers = taker :: it.takers) -> poll(taker.get).onCancel(cleanup)
            }.flatten
          }
        }

      def apply()(`<R`: >*<)(code: Seq[Any] => IO[Seq[Any]]): IO[Seq[Any]] =
        Deferred[IO, Seq[Any]].flatMap { taker =>
          IO.uncancelable { poll =>
            `<R`.modify {
              case it @ ><(_, offerers) if offerers.nonEmpty =>
                val i = random.nextInt(offerers.size)
                val ((names, offerer), rest) = offerers(i) -> (offerers.take(i) ++ offerers.drop(i+1))
                it.copy(offerers = rest) -> offerer.complete(Some(())).as(names)
              case it =>
                val cleanup = `<R`.update { it => it.copy(takers = it.takers.filter(_ ne taker)) }
                it.copy(takers = taker :: it.takers) -> poll(taker.get).onCancel(cleanup)
            }.flatten.flatMap {
              case it @ Seq(null, _*) => IO.pure(it)
              case it => (code andThen supervised)(it)
            }
          }
        }