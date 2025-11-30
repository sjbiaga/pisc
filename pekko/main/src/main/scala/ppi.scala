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
  import _root_.org.apache.pekko.actor.typed.scaladsl.Behaviors
  import _root_.org.apache.pekko.actor.typed.{ ActorRef, Behavior }

  import `Π-magic`.*


  /**
    * restriction aka new name
    */
  object ν:

    def apply(): Behavior[Request] =
      val i = Queue.empty[Request.Input]
      val o = Queue.empty[Request.Output]
      νActor(i, o)


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
    def apply(value: `()`*)
             (using ExecutionContext): Future[Option[Unit]] =
      Future { a ! Output(value.map(_.name), null, None) }.map(_ => Some(()))

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`*)(code: => Future[Any])
             (using ExecutionContext): Future[Option[Unit]] =
      for
        _      <- Future.unit
        promise = Promise[Unit]
        _      <- Future { a ! Output(value.map(_.name), promise, Some(code)) }
        _      <- promise.future
      yield
        Some(())

    /**
      * positive prefix i.e. input
      */
    def apply()(arity: Int)(using ExecutionContext): Future[Seq[`()`]] =
      for
        _      <- Future.unit
        promise = Promise[Seq[`()`]]
        _      <- Future { a ! Input(arity, promise, None) }
        names  <- promise.future
      yield
        names

    /**
      * positive prefix i.e. input
      */
    def apply()(arity: Int)(code: Seq[Any] => Future[Seq[Any]])
                           (using ExecutionContext): Future[Seq[`()`]] =
      for
        _      <- Future.unit
        promise = Promise[Seq[`()`]]
        _      <- Future { a ! Input(arity, promise, Some(code)) }
        names  <- promise.future
      yield
        names

    override def toString: String = if name == null then "null" else name.toString


  private object `Π-magic`:

    import Request.*

    enum Request:
      private[`Π-magic`] case Execute(promise: Promise[Unit])
      private[`Π-magic`] case Compute(arity: Int, promise: Promise[Seq[`()`]], result: Try[Seq[Any]])
      case Input(arity: Int, wrap: Promise[Seq[`()`]], code: Option[Seq[Any] => Future[Seq[Any]]])
      case Output(names: Seq[Any], done: Promise[Unit], exec: Option[Future[Any]])

    type >< = ActorRef[Request]

    def νActor(i: Queue[Input], o: Queue[Output]): Behavior[Request] =

      Behaviors.receive[Request] {

        case (_, Execute(promise))                     =>
          promise.success(())
          Behaviors.same

        case (_, Compute(arity, promise, result))      =>
          promise.success(result.fold(_ => Seq.fill(arity)(null).map(`()`), _.map(`()`)))
          Behaviors.same

        case (context, it @ Output(names, done, exec)) =>

          i.dequeueOption.fold(νActor(i, o.enqueue(it))) {
            case (Input(arity, wrap, Some(code)), i) if names.head != null =>
              context.pipeToSelf(code(names))(Compute(arity, wrap, _))
              if exec.isDefined then context.pipeToSelf(exec.get)(_ => Execute(done))
              νActor(i, o)
            case (Input(_, wrap, _), i)                                    =>
              wrap.success(names.map(`()`))
              if exec.isDefined then context.pipeToSelf(exec.get)(_ => Execute(done))
              νActor(i, o)
          }

        case (context, it @ Input(arity, wrap, code))  =>

          o.dequeueOption.fold(νActor(i.enqueue(it), o)) {
            case (Output(names, done, exec), o) if names.head != null && code.isDefined =>
              context.pipeToSelf(code.get(names))(Compute(arity, wrap, _))
              if exec.isDefined then context.pipeToSelf(exec.get)(_ => Execute(done))
              νActor(i, o)
            case (Output(names, done, exec), o)                                         =>
              wrap.success(names.map(`()`))
              if exec.isDefined then context.pipeToSelf(exec.get)(_ => Execute(done))
              νActor(i, o)
          }

      }
