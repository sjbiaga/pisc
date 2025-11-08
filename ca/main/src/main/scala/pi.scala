/*
 * Copyright (c) 2023-2025 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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

  import _root_.cats.effect.IO
  import _root_.cats.effect.kernel.Deferred
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.{ Queue, Supervisor }
  import _root_.com.suprnation.actor.ActorContext
  import _root_.com.suprnation.actor.ActorRef.ActorRef
  import _root_.com.suprnation.actor.Actor.{ Actor, Receive }

  import `Π-magic`.*


  /**
    * Supervised [[code]].
    * @param code
    */
  private def exec[T](code: => IO[T]): IO[T] =
    Supervisor[IO](await = true)
      .use(_.supervise(code))
      .flatMap(_.join
                .flatMap
                { case Succeeded(it) => it
                  case _ => IO(null.asInstanceOf[T]) }
              )


  /**
    * restriction aka new name
    */
  object ν:

    def map[B](f: ActorRef[IO, Request] => B)(using ActorContext[IO, Null, Any]): IO[B] = flatMap(f andThen IO.pure)
    def flatMap[B](f: ActorRef[IO, Request] => IO[B])(using context: ActorContext[IO, Null, Any]): IO[B] =
      ( for
          q <- Queue.unbounded[IO, Any]
          r <- Queue.unbounded[IO, (Deferred[IO, `()`], Option[Any => IO[Any]])]
          a <- context.system.actorOf(νActor(q, r))
        yield
          f(a)
      ).flatten


  /**
    * silent transition
    */
  val τ: IO[Option[Unit]] = IO(Some(()))


  /**
    * prefix
    */
  implicit final class `()`(private val name: Any) extends AnyVal:

    import Request.*

    private def a = `()`[><]

    def ====(that: `()`) =
      try
        this.a eq that.a
      catch
        case _ =>
          this.name == that.name

    inline def unary_! : Boolean = name == null
    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()` = this

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`): IO[Option[Unit]] = (a ! Output(value.name)).as(Some(()))

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`)(code: => IO[Any]): IO[Option[Unit]] = apply(value) <* exec(code)

    /**
      * positive prefix i.e. input
      */
    def apply()(implicit sender: Option[ActorRef[IO, Nothing]]): IO[Unit] = a ! Input(None)

    /**
      * positive prefix i.e. input
      */
    def apply[T]()(code: T => IO[Any])(implicit sender: Option[ActorRef[IO, Nothing]]): IO[Unit] = a ! Input(Some(code.asInstanceOf[Any => IO[Any]]))

    override def toString: String = if name == null then "null" else name.toString


  private object `Π-magic`:

    enum Request:
      case Input(code: Option[Any => IO[Any]])
      case Output(name: Any)

    type >< = ActorRef[IO, Request]

    final class νActor(q: Queue[IO, Any], r: Queue[IO, (Deferred[IO, `()`], Option[Any => IO[Any]])]) extends Actor[IO, Request]:

      import Request.*

      override def receive: Receive[IO, Request] =

        case Output(name) =>
          for
            opt <- r.tryTake
            _   <- opt.fold(q.offer(name)) {
                     case (deferred, Some(code)) if name != null =>
                       (code andThen exec)(name).flatMap(`()` andThen deferred.complete)
                     case (deferred, _) =>
                       deferred.complete(`()`(name))
                   }
          yield
            ()

        case Input(code)  =>
          for
            deferred <- Deferred[IO, `()`]
            opt      <- q.tryTake
            _        <- opt.fold(r.offer(deferred -> code)) {
                          case name if name != null && code.isDefined =>
                            (code.get andThen exec)(name).flatMap(`()` andThen deferred.complete)
                          case name =>
                            deferred.complete(`()`(name))
                        }
            _        <- sender.get.widenRequest[Deferred[IO, `()`]] ! deferred
          yield
            ()
