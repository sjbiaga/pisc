/*
 * Copyright (c) 2023-2024 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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
package generator

import scala.meta._
import dialects.Scala3

import parser.Ambient.{ AST => _, _ }
import parser.Calculus._
import Meta._


object Program:

  extension(node: Pre | AST)

    def generate(using id: => String): List[Enumerator] =
      var * = List[Enumerator]()

      node match

        // COMPOSITION /////////////////////////////////////////////////////////

        case ∅ =>
          * = `_ <- IO.unit`

        case ∥(operand) =>
          * = operand.generate

        case it: ∥ =>
          val ios = it.components.foldLeft(List[Term]())(_ :+ _.generate)

          * = `_ <- *`(`NonEmptyList( *, … ).parTraverse(identity)`(ios*))

        ///////////////////////////////////////////////////////// composition //


        // SEQUENCE ////////////////////////////////////////////////////////////

        case `.`(end, it*) =>
          * = (it :+ end).foldLeft(*)(_ ++ _.generate)

        //////////////////////////////////////////////////////////// sequence //


        // RESTRICTION | PREFIXES //////////////////////////////////////////////

        case ν(names*) =>
          * = names.map { it => `* <- *`(it -> "ν") }.toList


        case τ(Some((Left(enums), _))) =>
          * = `_ <- *`("τ")
          * ++= enums

        case τ(Some((Right(term), _))) =>
          * = `_ <- *`("τ")
          * :+= `_ <- IO { * }`(term)

        case τ(_) =>
          * = `_ <- *`("τ")


        case `,.`() =>

        case `,.`(path*) =>
          * = `_ <- *`(Term.Apply(
                         Term.Apply(\("ζ"), Term.ArgClause(\(")(") :: Nil, None)),
                         Term.ArgClause(`ζ(op, *, …)`(path.head, path.tail) :: Nil, None)))


        case `()`(name, it) =>
          val term = Term.Apply(\("()"), Term.ArgClause(\(")(") :: Nil, None))
          * = it match
                case Some((Right(code), _)) =>
                  `* <- *`(name -> Term.Apply(term, Term.ArgClause(code::Nil, None)))
                case _ =>
                  `* <- *`(name -> term)

        ////////////////////////////////////////////// restriction | prefixes //


        ////// REPLICATION /////////////////////////////////////////////////////

        case !(Some(name), par) =>
          val υidυ = id

          val `!.(*).⋯` = `()`(name, None).generate :+ `_ <- *`(Term.Apply(\(υidυ),
                                                                           Term.ArgClause(\(name) :: Nil, None)))

          val it = Term.If(Term.ApplyUnary("!", name),
                           `IO.cede`,
                           `NonEmptyList( *, … ).parTraverse(identity)`(
                             par.generate,
                             `!.(*).⋯`
                           )
                   )

          * = `* <- *`(υidυ -> `IO { def *(*: )(): IO[Any] = …; * }`(υidυ -> name, it)) :: `!.(*).⋯`

        case !(_, par) =>
          val υidυ = id

          val it = `NonEmptyList( *, … ).parTraverse(identity)`(
                     par.generate,
                     `_ <- IO.unit` :: `_ <- *`(υidυ)
                   )

          * = `* <- *`(υidυ, `IO { lazy val *: IO[Any] = …; * }`(υidυ, it)) :: `_ <- *`(υidυ)

        ///////////////////////////////////////////////////////// replication //


        // AMBIENT /////////////////////////////////////////////////////////////

        case `[]`(amb, par) =>
          val ** = `_ <- *`(Term.Apply(\("}{"), Term.ArgClause(\(")(") :: \(amb) :: Nil, None)))

          * = `_ <- *`(`NonEmptyList( *, … ).parTraverse(identity)`(** ++ par.generate))

        ///////////////////////////////////////////////////////////// ambient //


        // GO //////////////////////////////////////////////////////////////////

        case `go.`(amb, par) =>
          val ** = `_ <- *`(Term.Apply(\("ζ"), Term.ArgClause(\(")(") :: \(amb) :: Nil, None)))

          * = `_ <- *`(`NonEmptyList( *, … ).parTraverse(identity)`(** ++ par.generate))

        ////////////////////////////////////////////////////////////////// go //


        // OUTPUT //////////////////////////////////////////////////////////////

        case <>(it) =>
          val term = Term.Apply(`<>(null)`, Term.ArgClause(\(")(") :: Nil, None))
          * = it match
                case Some((Right(code), _)) =>
                  `_ <- *`(Term.Apply(term, Term.ArgClause(code::Nil, None)))
                case _ =>
                  `_ <- *`(term)

        case <>(it, path*) =>
          val term = Term.Apply(
                       Term.Apply(\("<>"), Term.ArgClause(`ζ(op, *, …)`(path.head, path.tail) :: Nil, None)),
                       Term.ArgClause(\(")(") :: Nil, None))

          * = it match
                case Some((Right(code), _)) =>
                  `_ <- *`(Term.Apply(term, Term.ArgClause(code::Nil, None)))
                case _ =>
                  `_ <- *`(term)

        ////////////////////////////////////////////////////////////// output //


        // INSTANTIATION ///////////////////////////////////////////////////////

        case `⟦⟧`(_, variables, _par, assign) =>
          val ** = assign
            .map(_.map(Pat.Var(_) -> _)
                  .map(Enumerator.Val(_, _))
                  .toList
            ).getOrElse(Nil)

          val n = assign.map(_.size).getOrElse(0)

          val par = ( if variables.size == n
                      then
                        _par
                      else
                        ∥(`.`(_par, ν(variables.drop(n).toSeq*)))
                    )

          * = ** ++ par.generate

        case _: `{}` => ???

        /////////////////////////////////////////////////////// instantiation //


        // INVOCATION //////////////////////////////////////////////////////////

        case `(*)`(identifier, qual, params*) =>
          val args = params.map("`" + _ + "`")

          qual match
            case Nil =>
              * :+= `_ <- *`(s"""`$identifier`(`)(`)(${args.mkString(", ")})""".parse[Term].get)
            case _ =>
              * :+= `_ <- *`(s"""${qual.mkString(".")}.π.`$identifier`(`)(`)(${args.mkString(", ")})""".parse[Term].get)

        ////////////////////////////////////////////////////////// invocation //

      *


  final class Main:

    def apply(prog: List[Bind]): List[String] =
      val id = new helper.υidυ
      prog.map(_ -> _.generate(using id())).map(_.swap).map(defn(_)(_).toString)
