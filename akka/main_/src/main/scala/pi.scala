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

  import _root_.scala.util.{ Success, Try }
  import _root_.java.util.concurrent.atomic.AtomicBoolean
  import _root_.scala.concurrent.{ ExecutionContext, Future, Promise }
  import _root_.scala.collection.immutable.Queue
  import _root_.akka.actor.typed.scaladsl.{ ActorContext, Behaviors }
  import _root_.akka.actor.typed.{ ActorRef, Behavior }

  import `Π-magic`.*

  type Π = Either[Option[AtomicBoolean], Behavior[Π]]


  /**
    * restriction aka new name
    */
  object ν:

    def apply(): Behavior[Request] =
      val q = Queue.empty[(Promise[Boolean], Any)]
      val r = Queue.empty[(Promise[`()`], Option[Any => Future[Any]])]
      νActor(q, r)


  /**
    * silent transition
    */
  val τ: Future[Option[Unit]] = Future.successful(Some(()))


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
    def apply(value: `()`)
             (using ExecutionContext): Future[Option[Unit]] =
      for
        _      <- Future.unit
        stop    = Promise[Boolean]
        _       = a ! Output(stop, value.name)
        stop   <- stop.future
      yield
        if stop then None else Some(())

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`)(code: => Future[Any])
             (using ExecutionContext): Future[Option[Unit]] =
      for
        r <- apply(value)
        _ <- code
      yield
        r

    /**
      * positive prefix i.e. input
      */
    def apply()(using ExecutionContext): Future[`()`] =
      for
        _      <- Future.unit
        promise = Promise[`()`]
        _       = a ! Input(promise, None)
        name   <- promise.future
      yield
        name

    /**
      * positive prefix i.e. input
      */
    def apply[T]()(code: T => Future[T])
                  (using ExecutionContext): Future[`()`] =
      for
        _      <- Future.unit
        promise = Promise[`()`]
        _       = a ! Input(promise, Some(code.asInstanceOf[Any => Future[Any]]))
        name   <- promise.future
      yield
        name

    override def toString: String = if name == null then "null" else name.toString


  private object `Π-magic`:

    enum Request:
      private[`Π-magic`] case Compute(promise: Promise[`()`], result: Try[Any], stop: Promise[Boolean])
      case Input(promise: Promise[`()`], code: Option[Any => Future[Any]])
      case Output(stop: Promise[Boolean], name: Any)

    type >< = ActorRef[Request]

    def νActor(q: Queue[(Promise[Boolean], Any)], r: Queue[(Promise[`()`], Option[Any => Future[Any]])]): Behavior[Request] =
      import Request.*

      Behaviors.receive[Request] {

        case (_, Compute(promise, result, stop)) =>
          promise.success(result.fold(_ => `()`(null), `()`))
          stop.success(result.fold(_ => true, _ == null))
          Behaviors.same

        case (context, Output(stop, name)) =>

          r.dequeueOption.fold(νActor(q.enqueue(stop -> name), r)) {
            case ((promise, Some(code)), r) if name != null =>
              context.pipeToSelf(code(name))(Compute(promise, _, stop))
              νActor(q, r)
            case ((promise, _), r) =>
              promise.success(`()`(name))
              stop.success(name == null)
              νActor(q, r)
          }

        case (context, Input(promise, code))  =>

          q.dequeueOption.fold(νActor(q, r.enqueue(promise -> code))) {
            case ((stop, name), q) if name != null && code.isDefined =>
              context.pipeToSelf(code.get(name))(Compute(promise, _, stop))
              νActor(q, r)
            case ((stop, name), q) =>
              promise.success(`()`(name))
              stop.success(name == null)
              νActor(q, r)
          }

      }
