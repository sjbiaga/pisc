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

package pisc
package emitter
package shared
package streams

import scala.annotation.tailrec

import scala.meta.*
import dialects.Scala3


abstract trait Meta extends shared.effects.Meta:

  val `: Stream[F, Any]` = Some(Type.Apply(\\("Stream"), Type.ArgClause(\\("F") :: \\("Any") :: Nil)))


  val `_ <- Stream.unit` =
    Enumerator.Generator(`* <- …`(), Term.Select("Stream", "unit"))


  def `*[F]`(* : Term) =
    Term.ApplyType(*, Type.ArgClause(\\("F") :: Nil))


  def `* <- Stream.eval(*)`(* : (String, Term)): Enumerator.Generator =
    `* <- *`(*._1 -> Term.Apply(Term.Select("Stream", "eval"), Term.ArgClause(*._2 :: Nil)))

  def `_ <- Stream.eval(*)`(* : Term): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), Term.Apply(Term.Select("Stream", "eval"), Term.ArgClause(* :: Nil)))

  private val `Stream.eval`: Term => Boolean =
    case Term.Select(Term.Name("Stream"), Term.Name("eval")) => true
    case Term.Apply(it, _) => `Stream.eval`(it)
    case Term.ApplyType(it, _) => `Stream.eval`(it)
    case _ => false

  def `Stream.eval(…)`(`…`: List[Enumerator]): List[Enumerator] =
    `…`.map {
      case it @ Enumerator.Generator(_, rhs) if `Stream.eval`(rhs) => it
      case it: Enumerator.Generator => it.copy(rhs = Term.Apply(Term.Select("Stream", "eval"), Term.ArgClause(it.rhs :: Nil)))
      case it => it
    }


  def `* <- Semaphore[F](…)`(* : String): Enumerator.Generator =
    `* <- Stream.eval(*)`(* -> Term.Apply(Term.ApplyType(\("Semaphore"), Type.ArgClause(\\("F") :: Nil)),
                                          Term.ArgClause(Lit.Int(1) :: Nil)))


  @tailrec
  final def `for *[F] yield ()`(* : Enumerator*): Term =
    if *.nonEmpty
    then
      if !(*.head.isInstanceOf[Enumerator.Generator])
      then
        `for *[F] yield ()`((`_ <- Stream.unit` +: *)*)
      else if *.size == 1
      then
        *.head match
          case Enumerator.Generator(Pat.Wildcard(), it: Term.ForYield) =>
            `for *[F] yield ()`(it.enums*)
          case Enumerator.Generator(Pat.Wildcard(), it) =>
            it
          case _ =>
            Term.ForYield(*.toList, Lit.Unit())
      else
        *.last match
          case Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name("Stream"), Term.Name("unit"))) =>
            `for *[F] yield ()`(*.init*)
          case _ =>
            Term.ForYield(*.toList, Lit.Unit())
    else
      `for *[F] yield ()`(`_ <- Stream.unit`)
