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

package masc
package emitter
package shared

import scala.meta.*
import dialects.Scala3

import parser.Ambient.{ AST, ζ, Λ }


abstract trait Meta:

  inline implicit def \(* : String): Term.Name = Term.Name(*)
  inline def \\(* : String): Type.Name = Type.Name(*)


  def `:`(name: String, clause: String): Option[Type.Apply] =
    Some(Type.Apply(Type.Name(name), Type.ArgClause(Type.Name(clause) :: Nil)))


  def `* <- …`(* : String*): Pat =
    if *.size == 0
    then
      Pat.Wildcard()
    else if *.size == 1
    then
      Pat.Var(*.head)
    else
      Pat.Tuple(*.map(\(_)).map(Pat.Var(_)).toList)


  def `* <- *`(* : (String, Term)): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(*._1), *._2)

  def `* = *`(* : (String, Term)): Enumerator.Val =
    Enumerator.Val(`* <- …`(*._1), *._2)


  def `_ <- *`(* : Term): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), *)


  val `<>(null)`: Term =
    Term.Apply(Term.Select("}{", "<>"),
               Term.ArgClause(Term.Apply(
                                Term.Select("Π", ")("),
                                Term.ArgClause(Lit.Null() :: Nil)
                              ) :: Nil)
    )


  def `ζ(op, *, …)`(head: AST, tail: Seq[AST]): Term =
    val next = if tail.isEmpty then \("None")
               else Term.Apply(\("Some"), Term.ArgClause(`ζ(op, *, …)`(tail.head, tail.tail) :: Nil))

    head match

      case ζ(op, amb) =>
        Term.Apply(\("ζ"),
                   Term.ArgClause(
                     Term.Apply(\("Some"),
                                Term.ArgClause(Term.Select("ζ-Op", op.toString) :: Nil)) ::
                     Term.Apply(\("Left"),
                                Term.ArgClause(\(amb) :: Nil)) ::
                     next :: Nil))

      case Λ(name) =>
        Term.Apply(Term.Select("Π", ")("),
                   Term.ArgClause(\(name) :: next :: Nil))

      case _ => ??? // neither name nor path - caught by parser


object Meta extends Meta
