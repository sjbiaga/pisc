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

import java.util.UUID

import scala.meta._
import dialects.Scala3

import parser.Ambient
import Ambient.{ AST => _, _ }
import parser.Calculus._
import parser.`Pre | AST`
import generator.Meta._


object Program:

  def apply(bind: List[Bind]): List[String] =
    bind.map { case (bind, par) => defn(bind, body(par)).toString }


  def body(node: `Pre | AST`): List[Enumerator] =
    var * = List[Enumerator]()

    node match

      // COMPOSITION ///////////////////////////////////////////////////////////

      case `∅` =>
       * = `_ <- IO.unit`

      case it: `|` if it.components.size > 1 =>
        val ios = it.components.foldLeft(List[Term]())(_ :+ body(_))

        * = `_ <- *`(`( *, … ).parMapN { (_, …) => }`(ios*))

      case `|`(operand, _*) =>
        * = body(operand)

      case _: `|` => ??? // impossible by syntax

      /////////////////////////////////////////////////////////// composition //


      // SEQUENCE //////////////////////////////////////////////////////////////

      case `.`(end, it*) =>
        * = (it :+ end).foldLeft(*)(_ ++ body(_))

      ////////////////////////////////////////////////////////////// sequence //


      // RESTRICTION | PREFIXES ////////////////////////////////////////////////

      case ν(names*) =>
        * = names.map { it => `* <- *`(it -> "ν") }.toList


      case τ(Some(Left(enums))) =>
        * = `_ <- *`("τ")
        * ++= enums

      case τ(Some(Right(term))) =>
        * = `_ <- *`("τ")
        * :+= `_ <- IO { * }`(term)

      case τ(_) =>
        * = `_ <- *`("τ")


      case `..`() =>

      case `..`(path*) =>
        * = `_ <- *`(Term.Apply(
                       Term.Apply(\("ζ"), Term.ArgClause(\(")(") :: Nil, None)),
                       Term.ArgClause(fold(path.head, path.tail*) :: Nil, None)))


      case `()`(it, amb) =>
        val uuid = id

        val term = Term.Apply(
                     Term.Apply(\("()"), Term.ArgClause(Nil, None)),
                     Term.ArgClause(\(uuid) :: Nil, None))

        * = List(
          `* <- *`(uuid -> Term.Apply(Term.Select("Π", "]["), Term.ArgClause(\(")(") :: Nil, None))),
          it match
            case Some(code) =>
              `* <- *`(amb -> Term.Apply(term, Term.ArgClause(code::Nil, None)))
            case _ =>
              `* <- *`(amb -> term)
        )

      //////////////////////////////////////////////// restriction | prefixes //


      ////// REPLICATION ///////////////////////////////////////////////////////

      case `!`(Some(name), par) =>
        val uuid = id

        val `!.(*).⋯` = body(`()`(None, name)) :+ `_ <- *`(s"$uuid($name)".parse[Term].get)

        val it =
          `for * yield ()`(
            `_ <- *` {
              Term.If(Term.ApplyUnary("!", name),
                      `IO.cede`,
                      `( *, … ).parMapN { (_, …) => }`(
                        `for * yield ()`(body(par)*),
                        `for * yield ()`(`!.(*).⋯`*)
                      )
              )
            }
          )

        * :+= `* <- *`(uuid -> `IO { def *(*: )(): IO[Unit] = …; * }`(uuid -> name, it))
        * ++= `!.(*).⋯`

      case `!`(_, par) =>
        val uuid = id

        val it =
          `for * yield ()` {
            `_ <- *` {
              `( *, … ).parMapN { (_, …) => }`(
                body(par),
                `for * yield ()`(`_ <- IO.unit`, `_ <- *`(uuid))
              )
            }
          }

        * :+= `* <- *`(uuid, `IO { lazy val *: IO[Unit] = …; * }`(uuid, it))
        * :+= `_ <- *`(uuid)

      /////////////////////////////////////////////////////////// replication //


      // AMBIENT ///////////////////////////////////////////////////////////////

      case `[]`(amb, par) =>
        val ** = `_ <- *`(Term.Apply(\("}{"), Term.ArgClause(\(")(") :: \(amb) :: Nil, None))) :: Nil

        * = `_ <- *`(`( *, … ).parMapN { (_, …) => }`(`IO.cede`, ** ++ body(par)))

      /////////////////////////////////////////////////////////////// AMBIENT //

      // OUTPUT ////////////////////////////////////////////////////////////////

      case `<>`(it) =>
        val uuid = id

        val term = Term.Apply(`<>(null)`, Term.ArgClause(\(uuid) :: Nil, None))

        * = List(
          `* <- *`(uuid -> Term.Apply(Term.Select("Π", "]["), Term.ArgClause(\(")(") :: Nil, None))),
          it match
            case Some(code) =>
              `_ <- *`(Term.Apply(term, Term.ArgClause(code::Nil, None)))
            case _ =>
              `_ <- *`(term)
        )

      case `<>`(it, path*) =>
        val uuid = id

        val term = Term.Apply(
                     Term.Apply(\("<>"), Term.ArgClause(fold(path.head, path.tail*) :: Nil, None)),
                     Term.ArgClause(\(uuid) :: Nil, None))

        * = List(
          `* <- *`(uuid -> Term.Apply(Term.Select("Π", "]["), Term.ArgClause(\(")(") :: Nil, None))),
          it match
            case Some(code) =>
              `_ <- *`(Term.Apply(term, Term.ArgClause(code::Nil, None)))
            case _ =>
              `_ <- *`(term)
        )

      //////////////////////////////////////////////////////////////// output //


      // INVOCATION ////////////////////////////////////////////////////////////

      case `(*)`(identifier, qual, params*) =>
        if qual.isEmpty
        then
          * :+= `_ <- *`(s"`$identifier`(`)(`)(${params.mkString(", ")})".parse[Term].get)
        else
          * :+= `_ <- *`(s"${qual.mkString(".")}.`π`.`$identifier`(`)(`)(${params.mkString(", ")})".parse[Term].get)

      //////////////////////////////////////////////////////////// invocation //

    *

  def fold(head: Ambient.AST, tail: Ambient.AST*): Term =
    val next = if tail.isEmpty then \("None")
               else Term.Apply(\("Some"), Term.ArgClause(fold(tail.head, tail.tail*) :: Nil, None))
    head match
      case ζ(op, amb) =>
        Term.Apply(\("ζ"),
                   Term.ArgClause(
                     Term.Apply(\("Some"),
                                Term.ArgClause(Term.Select("ζ-Op", op.toString) :: Nil, None))
                     :: \(amb) :: next :: Nil, None))

      case Λ(name) =>
        Term.Apply(\("ζ"), Term.ArgClause(\("None") :: \(name) :: next :: Nil, None))

      case _ => ??? // neither name nor path - caught by parser

  def id = "_" + UUID.randomUUID.toString.replaceAll("-", "_")
