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

package masc
package emitter

import scala.annotation.tailrec

import scala.meta.*
import dialects.Scala3

import parser.Calculus.`(*)`
import parser.Ambient.{ AST, ζ, Λ }


object Meta:

  def defn(body: Term): `(*)` => Defn.Def =
    case `(*)`("Main", _) =>
      Defn.Def(Nil,
               "Main", `String*`("args"), `: IO[Any]`,
               body)
    case `(*)`(identifier, _, params*) =>
      Defn.Def(Nil,
               identifier, `(…)`(params*), `: IO[Any]`,
               body)

  inline implicit def \(* : Enumerator): List[Enumerator] = * :: Nil

  implicit def \(* : List[Enumerator]): Term =
    if *.nonEmpty then `for * yield ()`(* *)
    else \(`_ <- IO.unit`)

  inline implicit def \(* : String): Term.Name = Term.Name(*)


  def `String*`(* : String) =
    Member.ParamClauseGroup(
      Type.ParamClause(Nil),
      Term.ParamClause(Term.Param(Nil, \(")("), `:`("IOLocal", ")("), None) ::
                       Term.Param(Nil, \("}{"), Some(Type.Select("Π", Type.Name("}{"))), None) :: Nil,
                       None) ::
      Term.ParamClause(Term.Param(Nil, *, Some(Type.Repeated(Type.Name("String"))), None) :: Nil) ::
      Term.ParamClause(Term.Param(Mod.Using() :: Nil, Name.Anonymous(), Some(Type.Select("}{", Type.Name("]["))), None) ::
                       Term.Param(Mod.Using() :: Nil, Name.Anonymous(), Some(Type.Select(Term.Select("}{", "stm"), Type.Name("TSemaphore"))), None) :: Nil,
                       Some(Mod.Using())) ::
      Nil
    ) :: Nil

  def `(…)`(* : String*) =
    Member.ParamClauseGroup(
      Type.ParamClause(Nil),
      Term.ParamClause(Term.Param(Nil, \(")("), `:`("IOLocal", ")("), None) ::
                       Term.Param(Nil, \("}{"), Some(Type.Select("Π", Type.Name("}{"))), None) :: Nil,
                       None) ::
      Term.ParamClause(*
                        .map(\(_))
                        .map(Term.Param(Nil, _, Some(Type.Name(")(")), None))
                        .toList,
                       None) ::
      Term.ParamClause(Term.Param(Mod.Using() :: Nil, Name.Anonymous(), Some(Type.Select("}{", Type.Name("]["))), None) ::
                       Term.Param(Mod.Using() :: Nil, Name.Anonymous(), Some(Type.Select(Term.Select("}{", "stm"), Type.Name("TSemaphore"))), None) :: Nil,
                       Some(Mod.Using())) ::
      Nil
    ) :: Nil


  def `:`(name: String, clause: String): Option[Type.Apply] =
    Some(Type.Apply(Type.Name(name), Type.ArgClause(Type.Name(clause) :: Nil)))


  val `: IO[Any]` = `:`("IO", "Any")


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

  def `* = *`(* : (Pat, Term)): Enumerator.Val =
    Enumerator.Val(*._1, *._2)

  val `_ <- IO.unit` = `_ <- IO.*`("unit")

  val `IO.cede` = Term.Select("IO", "cede")


  def `_ <- *`(* : Term): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), *)


  def `_ <- IO.*`(* : String): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), Term.Select("IO", *))

  def `_ <- IO { * }`(* : Term): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(),
                         Term.Apply(\("IO"),
                                    Term.ArgClause(Term.Block(* :: Nil) :: Nil)))

  def `_ <- IO.sleep(*.…)`(* : Long, `…`: String): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(),
                         Term.Apply(Term.Select("IO", "sleep"),
                                    Term.ArgClause(Term.Select(Lit.Long(*), `…`) :: Nil)))


  @tailrec
  def `for * yield ()`(* : Enumerator*): Term =
    if *.nonEmpty
    then
      if !(*.head.isInstanceOf[Enumerator.Generator])
      then
        `for * yield ()`((`_ <- IO.unit` +: *)*)
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
          case Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name("IO"), Term.Name("unit" | "cede"))) =>
            `for * yield ()`(*.init*)
          case _ =>
            Term.ForYield(*.toList, Lit.Unit())
    else
      `for * yield ()`(`_ <- IO.unit`)


  def `List( *, … ).parSequence`(* : Term*): Term =
    *.flatMap {
      case Term.Select(Term.Name("IO"), Term.Name("unit" | "cede")) => None
      case it => Some(it)
    } match
      case Nil => `IO.cede`
      case it => Term.Select(Term.Apply(\("πLs"), Term.ArgClause(it.toList)), "πparSequence")


  def `IO { def *(*: )(): IO[Any] = …; * }`(* : (String, String), `…`: Term): Term =
    Term.Apply(\("IO"),
               Term.ArgClause(
                 Term.Block(
                   Defn.Def(Nil,
                            *._1,
                            Member.ParamClauseGroup(Type.ParamClause(Nil),
                                                    Term.ParamClause(Term.Param(Nil,
                                                                                *._2,
                                                                                Some(Type.Name(")(")),
                                                                                None) :: Nil, None) :: Nil) :: Nil,
                            `: IO[Any]`,
                             `…`
                   ) :: \(*._1) :: Nil
                 ) :: Nil
               )
    )


  def `IO { lazy val *: IO[Any] = …; * }`(* : String, `…`: Term): Term =
    Term.Apply(\("IO"),
               Term.ArgClause(Term.Block(
                                Defn.Val(Mod.Lazy() :: Nil,
                                         `* <- …`(*) :: Nil,
                                         `: IO[Any]`,
                                         `…`
                                ) :: \(*) :: Nil
                              ) :: Nil
               )
    )


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
