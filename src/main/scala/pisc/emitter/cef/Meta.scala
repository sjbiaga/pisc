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
package cef

import scala.annotation.tailrec

import scala.meta.*
import dialects.Scala3


object Meta extends emitter.shared.effects.Meta:

  def `*.flatMap { null else … }`(* : Term, `…`: List[Enumerator])(implicit υidυ: String): Term =
    Term.Apply(
      Term.Select(*, "flatMap"),
      Term.ArgClause(
        Term.Block(
          Term.Function(
            Term.ParamClause(Term.Param(Nil, υidυ, None, None) :: Nil),
            `if * then … else …`(
              Term.ApplyInfix(\(υidυ), \("eq"),
                              Type.ArgClause(Nil),
                              Term.ArgClause(Lit.Null()::Nil)),
              `IO.cede`,
              `for * yield ()`(`…`*)
            )
          ) :: Nil) :: Nil)
    )

  def `*.flatMap { null else … }`(* : Term, `…`: List[Enumerator])(par: String)(implicit υidυ: String): Term =
    Term.Apply(
      Term.Select(*, "flatMap"),
      Term.ArgClause(
        Term.Block(
          Term.Function(
            Term.ParamClause(Term.Param(Nil, par, None, None) ::
                             Term.Param(Nil, υidυ, None, None) :: Nil),
            `if * then … else …`(
              Term.ApplyInfix(\(υidυ), \("eq"),
                              Type.ArgClause(Nil),
                              Term.ArgClause(Lit.Null()::Nil)),
              `IO.cede`,
              `for * yield ()`(`…`*)
            )
          ) :: Nil) :: Nil)
    )


  private def `(*)`(* : Term): Option[Term] =
    * match
      case Term.Select(Term.Name("IO"), Term.Name("unit" | "cede")) => None
      case _ => Some(*)

  @tailrec
  def `List( *, … ).parSequence`(* : Term*): Term =
    if *.exists {
      case Term.Select(Term.Apply(Term.Name("πLs"), _), Term.Name("πparSequence")) => true
      case _ => false
    } then
      `List( *, … ).parSequence`((
        *.flatMap {
          case Term.Select(Term.Name("IO"), Term.Name("unit" | "cede")) => None
          case Term.Select(Term.Apply(Term.Name("πLs"), ls), Term.Name("πparSequence")) =>
            ls.flatMap {
              case Term.Select(Term.Name("IO"), Term.Name("unit" | "cede")) => None
              case it => Some(it)
            }
          case it => Some(it)
        })*)
      else
        Term.Select(Term.Apply(\("πLs"), Term.ArgClause(*.flatMap(`(*)`).toList)), "πparSequence")
