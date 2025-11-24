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

package object sΠ:

  import _root_.scala.collection.immutable.{ Map, Set }

  import _root_.scala.util.{ Success, Try }
  import _root_.scala.concurrent.{ ExecutionContext, Future, Promise }
  import _root_.scala.collection.immutable.Queue
  import _root_.org.apache.pekko.actor.typed.scaladsl.{ ActorContext, Behaviors }
  import _root_.org.apache.pekko.actor.typed.{ ActorRef, Behavior }

  import `Π-loop`.Loop.*
  import `Π-loop`.%
  import `Π-stats`.Rate

  export `Π-magic`.><
  import `Π-magic`.*


  type `Π-Map`[K, +V] = Map[K, V]

  type `Π-Set`[A] = Set[A]


  private def exclude(key: String)
                     (using % : %)
                     (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]]) =
    if `π-elvis`.contains(key)
    then
      % ! Exclude(`π-elvis`(key))


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
  object τ:

    def apply(rate: Rate)(key: String)
             (using % : %)
             (using ExecutionContext)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): Future[java.lang.Double] =
      for
        _     <- Future { exclude(key) }
        cancel = Promise[Option[Double]]
        _     <- Future { % ! Enqueue(^, key, cancel -> (System.nanoTime, (new Object, None, rate))) }
        delay <- cancel.future
      yield
        if delay eq None then null else delay.get


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
    def apply(rate: Rate, value: `()`)(key: String)
             (using % : %)
             (using ExecutionContext)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): Future[java.lang.Double] =
      for
        _     <- Future { exclude(key) }
        cancel = Promise[Option[Double]]
        _     <- Future { % ! Enqueue(^, key, cancel -> (System.nanoTime, (a, Some(false), rate))) }
        delay <- cancel.future
        _     <- if (delay eq None)
                 then
                   Future.unit
                 else
                   Future { a ! Output(value.name, null, None) }
      yield
        if delay eq None then null else delay.get

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`)(key: String)(code: => Future[Any])
             (using % : %)
             (using ExecutionContext)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): Future[java.lang.Double] =
      for
        _     <- Future { exclude(key) }
        cancel = Promise[Option[Double]]
        _     <- Future { % ! Enqueue(^, key, cancel -> (System.nanoTime, (a, Some(false), rate))) }
        delay <- cancel.future
        _     <- if (delay eq None)
                 then
                   Future.unit
                 else
                   for
                      _      <- Future.unit
                      promise = Promise[Unit]
                      _      <- Future { a ! Output(value.name, promise, Some(code)) }
                      _      <- promise.future
                   yield
                     ()
      yield
        if delay eq None then null else delay.get

    /**
      * positive prefix i.e. input
      */
    def apply(rate: Rate)(key: String)
             (using % : %)
             (using ExecutionContext)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): Future[(`()`, java.lang.Double)] =
      for
        _     <- Future { exclude(key) }
        cancel = Promise[Option[Double]]
        _     <- Future { % ! Enqueue(^, key, cancel -> (System.nanoTime, (a, Some(true), rate))) }
        delay <- cancel.future
        name  <- if (delay eq None)
                 then
                   Future.successful(new `()`(null))
                 else
                   for
                     _      <- Future.unit
                     promise = Promise[`()`]
                     _      <- Future { a ! Input(promise, None) }
                     name   <- promise.future
                   yield
                     name
      yield
        (name, if delay eq None then null else delay.get)

    /**
      * positive prefix i.e. input
      */
    def apply[T](rate: Rate)(key: String)(code: T => Future[T])
                (using % : %)
                (using ExecutionContext)
                (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): Future[(`()`, java.lang.Double)] =
      for
        _     <- Future { exclude(key) }
        cancel = Promise[Option[Double]]
        _     <- Future { % ! Enqueue(^, key, cancel -> (System.nanoTime, (a, Some(true), rate))) }
        delay <- cancel.future
        name  <- if (delay eq None)
                 then
                   Future.successful(new `()`(null))
                 else
                   for
                     _      <- Future.unit
                     promise = Promise[`()`]
                     _      <- Future { a ! Input(promise, Some(code.asInstanceOf[Any => Future[Any]])) }
                     name   <- promise.future
                   yield
                     name
      yield
        (name, if delay eq None then null else delay.get)

    override def toString: String = if name == null then "null" else name.toString


  private object `Π-magic`:

    import Request.*

    enum Request:
      private[`Π-magic`] case Execute(promise: Promise[Unit])
      private[`Π-magic`] case Compute(promise: Promise[`()`], result: Try[Any])
      case Input(wrap: Promise[`()`], code: Option[Any => Future[Any]])
      case Output(name: Any, done: Promise[Unit], exec: Option[Future[Any]])

    type >< = ActorRef[Request]

    def νActor(i: Queue[Input], o: Queue[Output]): Behavior[Request] =

      Behaviors.receive[Request] {

        case (_, Execute(promise))                    =>
          promise.success(())
          Behaviors.same

        case (_, Compute(promise, result))            =>
          promise.success(result.fold(_ => `()`(null), `()`))
          Behaviors.same

        case (context, it @ Output(name, done, exec)) =>

          i.dequeueOption.fold(νActor(i, o.enqueue(it))) {
            case (Input(wrap, Some(code)), i) if name != null =>
              context.pipeToSelf(code(name))(Compute(wrap, _))
              if exec.isDefined then context.pipeToSelf(exec.get)(_ => Execute(done))
              νActor(i, o)
            case (Input(wrap, _), i)                          =>
              wrap.success(`()`(name))
              if exec.isDefined then context.pipeToSelf(exec.get)(_ => Execute(done))
              νActor(i, o)
          }

        case (context, it @ Input(wrap, code))        =>

          o.dequeueOption.fold(νActor(i.enqueue(it), o)) {
            case (Output(name, done, exec), o) if name != null && code.isDefined =>
              context.pipeToSelf(code.get(name))(Compute(wrap, _))
              if exec.isDefined then context.pipeToSelf(exec.get)(_ => Execute(done))
              νActor(i, o)
            case (Output(name, done, exec), o)                                   =>
              wrap.success(`()`(name))
              if exec.isDefined then context.pipeToSelf(exec.get)(_ => Execute(done))
              νActor(i, o)
          }

      }
