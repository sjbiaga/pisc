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
package effects

import scala.annotation.tailrec

import scala.meta.*
import dialects.Scala3


abstract trait Meta extends shared.Meta:

  protected lazy val \ = ""

  inline implicit def \(* : Enumerator): List[Enumerator] = * :: Nil

  inline implicit def \\(* : Enumerator): Term = \(*)

  implicit def \(* : List[Enumerator]): Term =
    if *.nonEmpty then `for * yield ()`(* *)
    else \(`_ <- \\.unit`)


  lazy val `_ <- \\.unit` = `_ <- \\.*`("unit")


  def `_ <- \\.*`(* : String): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), Term.Select(\, *))


  @tailrec
  final def `for * yield ()`(* : Enumerator*): Term =
    if *.nonEmpty
    then
      if !(*.head.isInstanceOf[Enumerator.Generator])
      then
        `for * yield ()`((`_ <- \\.unit` +: *)*)
      else if *.size == 1
      then
        *.head match
          case Enumerator.Generator(Pat.Wildcard(), it: Term.ForYield) =>
            `for * yield ()`(it.enums*)
          case Enumerator.Generator(Pat.Wildcard(), it) =>
            it
          case _ =>
            Term.ForYield(*.toList, Lit.Unit())
      else
        *.last match
          case Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(`\\`), Term.Name("unit" | "cede")))
             | Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("pure")), Lit.Unit() :: Nil)) =>
            `for * yield ()`(*.init*)
          case _ =>
            Term.ForYield(*.toList, Lit.Unit())
    else
      `for * yield ()`(`_ <- \\.unit`)


  def `_ <- *.acquire`(* : String): Enumerator =
    Enumerator.Generator(`* <- …`(), Term.Select(*, "acquire"))

  def `_ <- *.release`(* : String): Enumerator =
    Enumerator.Generator(`* <- …`(), Term.Select(*, "release"))

  def `* <- Semaphore(…)`(* : String, `…`: Int = 1): Enumerator =
    Enumerator.Generator(`* <- …`(*),
                         Term.Apply(Term.ApplyType(\("Semaphore"), Type.ArgClause(\\(\) :: Nil)),
                                    Term.ArgClause(Lit.Int(`…`) :: Nil)
                         )
    )
