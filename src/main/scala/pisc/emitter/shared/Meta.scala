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

import scala.annotation.tailrec

import scala.meta.*
import dialects.Scala3

import parser.Calculus.{ λ, `(*)` }


abstract trait Meta:

  def ====(lhs: λ, rhs: λ): Term =
    Term.ApplyInfix(lhs.toTerm,
                    \("===="),
                    Type.ArgClause(Nil),
                    Term.ArgClause(rhs.toTerm :: Nil))


  inline implicit def \(* : String): Term.Name = Term.Name(*)


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

  def `* = *: *`(* : (String, String, Type)): Enumerator.Val =
    Enumerator.Val(Pat.Typed(Pat.Var(*._1), *._3),
                   Term.ApplyType(
                     Term.Select(*._2, "()"),
                     Type.ArgClause(*._3 :: Nil)
                   ))

  def `* = *: * …`(* : (String, String, Type, Type)): Enumerator.Val =
    Enumerator.Val(Pat.Typed(Pat.Var(*._1), *._3),
                   Term.Select(
                     Term.Select(
                       Term.Select(
                         Term.Apply(
                           Term.ApplyType(
                             \("refineV"),
                             Type.ArgClause(*._4 :: Nil)
                           ),
                           Term.ArgClause(
                             Term.ApplyType(
                               Term.Select(*._2, "()"),
                               Type.ArgClause(*._3 :: Nil)
                             ) :: Nil)
                         ),
                         "right"
                       ),
                       "get"
                     ),
                     "value"
                   ))

  def `* :: … :: * = *`(* : (String, String), `…`: String*) =
    def pat(** : String*): Pat =
      val head =
        if **.head.isEmpty then
          Pat.Wildcard()
        else
          Pat.Var(**.head)
      if **.size == 1
      then
        head
      else
        Pat.ExtractInfix(head, \(*._1), Pat.ArgClause(pat(**.tail*) :: Nil))
    Enumerator.Val(pat(`…`*), *._2)


  def `_ <- *`(* : Term): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), *)

  def `_ = *`(* : Term): Enumerator.Val =
    Enumerator.Val(`* <- …`(), *)


  def `if * then … else …`(* : Term, `…`: Term*): Term.If =
    Term.If(*, `…`(0), `…`(1), Nil)


  def `val * = *: *`(* : (String, String, Type)): Defn.Val =
    Defn.Val(Nil,
             Pat.Var(*._1) :: Nil,
             Some(*._3),
             Term.ApplyType(
               Term.Select(*._2, "()"),
               Type.ArgClause(*._3 :: Nil)
             )
    )

  def `val * = *: * …`(* : (String, String, Type, Type)): Defn.Val =
    Defn.Val(Nil,
             Pat.Var(*._1) :: Nil,
             Some(*._3),
             Term.Select(
               Term.Select(
                 Term.Select(
                   Term.Apply(
                     Term.ApplyType(\("refineV"),
                                    Type.ArgClause(*._4 :: Nil)
                     ),
                     Term.ArgClause(
                       Term.ApplyType(
                         Term.Select(*._2, "()"),
                         Type.ArgClause(*._3 :: Nil)) :: Nil)
                   ),
                   "right"
                 ),
                 "get"
               ),
               "value"
             )
    )


object Meta extends Meta:

  val `()(null)`: Term =
    Term.Apply(\("()"), Term.ArgClause(Lit.Null() :: Nil))
