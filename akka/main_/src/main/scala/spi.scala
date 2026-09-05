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

package object sΠ:

  import _root_.scala.collection.immutable.{ Map, Queue, Set }

  import _root_.scala.concurrent.{ ExecutionContext, Future, Promise }

  import _root_.scala.reflect.{ ClassTag, classTag }

  import _root_.scala.util.Success

  import _root_.akka.actor.typed.scaladsl.Behaviors
  import _root_.akka.actor.typed.Behavior

  import `Π-loop`.Loop.*
  import `Π-loop`.%
  import `Π-stats`.Rate


  type `Π-Map`[K, +V] = Map[K, V]

  type `Π-Set`[A] = Set[A]


  private def exclude(key: String)
                     (using % : %)
                     (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]]) =
    `π-elvis`.get(key) match
      case Some(it) if it.nonEmpty => % ! Exclude(it)
      case _ =>

  inline def `π-exclude`(enabled: String*)
                        (using % : %) =
    if enabled.nonEmpty then % ! Exclude(Set.from(enabled))


  /**
    * restriction aka new name
    */
  object ν:

    def apply(): Behavior[Unit] =
      Behaviors.receiveMessage { _ => Behaviors.same }


  /**
    * silent transition
    */
  object τ:

    private val `new {}` = new {}

    def apply(rate: Rate)(key: String)
             (using % : %)
             (using ExecutionContext)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): Future[java.lang.Double] =
      for
        _     <- Future { exclude(key) }
        cancel = Promise[Option[(Promise[`()`], Double)]]()
        _     <- Future { % ! Enqueue(^, key, cancel -> (System.currentTimeMillis, (`new {}`, None, rate))) }
        opt   <- cancel.future
        delay <- if (opt eq None)
                 then
                   Future.successful(null: java.lang.Double)
                 else
                   val (_, delay) = opt.get
                   Future.successful(java.lang.Double(delay))
      yield
        delay


  /**
    * prefix
    */
  implicit final class `()`(private val name: Any) extends AnyVal:

    def ====(that: `()`) =
      this.name == that.name

    inline def unary_! : Boolean = name == null
    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()` = this

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(rate: Rate, value: => S)(key: String)
                                     (using DummyImplicit)
                                     (using %)
                                     (using ExecutionContext)
                                     (using `Π-Map`[String, `Π-Set`[String]], String): Future[java.lang.Double] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(rate, value.asInstanceOf[`()`])(key)
      else
        apply(false)(rate, Future(value))(key)

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(rate: Rate, value: => S)(key: String)(code: => Future[Any])
                                    (using DummyImplicit)
                                    (using %)
                                    (using ExecutionContext)
                                    (using `Π-Map`[String, `Π-Set`[String]], String): Future[java.lang.Double] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(rate, value.asInstanceOf[`()`])(key)(code)
      else
        apply(true)(rate, Future(value))(key)(code)

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(rate: Rate, value: => Future[S])(key: String)
                                     (using %)
                                     (using ExecutionContext)
                                     (using `Π-Map`[String, `Π-Set`[String]], String): Future[java.lang.Double] =
      if classTag[S].runtimeClass eq getClass
      then
        Future(value.asInstanceOf[Future[`()`]].flatMap(apply(rate, _)(key))).flatten
      else
        Future(value.map(new `()`(_)).flatMap(apply(rate, _)(key))).flatten

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(rate: Rate, value: => Future[S])(key: String)(code: => Future[Any])
                                    (using %)
                                    (using ExecutionContext)
                                    (using `Π-Map`[String, `Π-Set`[String]], String): Future[java.lang.Double] =
      if classTag[S].runtimeClass eq getClass
      then
        Future(value.asInstanceOf[Future[`()`]].flatMap(apply(rate, _)(key)(code))).flatten
      else
        Future(value.map(new `()`(_)).flatMap(apply(rate, _)(key)(code))).flatten

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
        cancel = Promise[Option[(Promise[`()`], Double)]]()
        _     <- Future { % ! Enqueue(^, key, cancel -> (System.currentTimeMillis, (`()`[{}], Some(Left(())), rate))) }
        opt   <- cancel.future
        delay <- if (opt eq None)
                 then
                   Future.successful(null: java.lang.Double)
                 else
                   val (result, delay) = opt.get
                   Future.successful(result.complete(Success(value))).map(_ => java.lang.Double(delay))
      yield
        delay

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
        cancel = Promise[Option[(Promise[`()`], Double)]]()
        _     <- Future { % ! Enqueue(^, key, cancel -> (System.currentTimeMillis, (`()`[{}], Some(Left(())), rate))) }
        opt   <- cancel.future
        delay <- if (opt eq None)
                 then
                   Future.successful(null: java.lang.Double)
                 else
                   val (result, delay) = opt.get
                   Future.successful(result.complete(Success(value))).flatMap(_ => code).map(_ => java.lang.Double(delay))
      yield
        delay

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
        cancel = Promise[Option[(Promise[`()`], Double)]]()
        result = Promise[`()`]()
        _     <- Future { % ! Enqueue(^, key, cancel -> (System.currentTimeMillis, (`()`[{}], Some(Right(result)), rate))) }
        opt   <- cancel.future
        n_d   <- if (opt eq None)
                 then
                   Future.successful(new `()`(null) -> (null: java.lang.Double))
                 else
                   val (_, delay) = opt.get
                   result.future.map(_ -> java.lang.Double(delay))
      yield
        n_d

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
        cancel = Promise[Option[(Promise[`()`], Double)]]()
        result = Promise[`()`]()
        _     <- Future { % ! Enqueue(^, key, cancel -> (System.currentTimeMillis, (`()`[{}], Some(Right(result)), rate))) }
        opt   <- cancel.future
        n_d   <- if (opt eq None)
                 then
                   Future.successful(new `()`(null) -> (null: java.lang.Double))
                 else
                   val (_, delay) = opt.get
                   result.future.map(_.name).flatMap {
                     case null  => null
                     case it: T => code(it)
                   }.map(new `()`(_) -> java.lang.Double(delay))
      yield
        n_d

    override def toString: String = if name == null then "null" else name.toString
