/*
 * Copyright (c) 2023-2023 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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

import java.util.UUID

import scala.collection.mutable.{ HashMap => Map }

import Calculus.{ rate => _, _ }
import Program._


final class Program(indent: String = "  "):

  def apply(bind: List[Bind]): String = bind
    .map { case (bind, sum) =>
      val code = (false -> indent) -> ("", "") -> false
      defn(code)(bind, sum)
    }.mkString("\n\n")

  private def defn(code: Code)(bind: Call, sum: Sum): String =
    var (((comprehension, prefix1), (before1, after1)), _) = code
    val identifier = bind.identifier.asSymbol.name
    val params = bind.params.map(_.asSymbol.name).map { _.toString + ": Name" }

    val prefix2 =
      s"${prefix1}     ${" ".repeat(identifier.length)} "

    before1 +=
      s"${prefix1}def `$identifier`(${params.mkString(", ")})(^ : String)\n" +
      s"${prefix2}(using % : %, / : /, + : +, - : -): IO[Unit] =\n"

    val prefix =
      s"${prefix1}$indent"

    val cp = comprehension -> prefix

    val (_, (before, after)) = body(cp -> (before1, after1) -> init(sum), sum)

    before + after

  private def body(code: Code, node: AST): (String, (String, String)) =
    //                                      ^^^^^^   ^^^^^^  ^^^^^^
    //                              [Match] prefix,  before, after
    var ((cp @ (comprehension, prefix1), (before1, after1)), setup) = code

    node match

      // SUMMATION /////////////////////////////////////////////////////////////

      case it @ Sum(enabled, _, _, _*) =>
        val prefix2 = prefix1 + (if comprehension then "" else indent)

        val before2 =
          (if comprehension then "" else s"${prefix1}for\n") +
          s"${prefix2}_ <- IO.unit\n" +
          (if setup && enabled.nonEmpty then s"${add(enabled, prefix2)}\n" else "") +
          (if setup && enabled.nonEmpty then s"${excl(enabled, prefix2)}\n" else "") +
          s"${prefix2}_ <- (\n"
        val separator =
          s"${prefix2}     ,\n"
        val after2 =
          s"${prefix2}     ).parMapN { (${"_, ".repeat(it.choices.size-1)}_) => }\n" +
          (if comprehension then "" else s"${prefix1}yield\n${prefix2}()\n")

        val prefix =
          s"${prefix2}     ${indent}"

        cp = false -> prefix

        val (before3, after3) = it
          .choices
          .zipWithIndex
          .foldLeft((before2, "")) { case ((before, after), (operand, i)) =>
            val (_, (before3, after3)) = body(cp -> (before, after) -> false, operand)
            (before3 + after3 + (if i < it.choices.size - 1 then separator else after2), "")
          }

        prefix1 -> (before1 + before3, after3 + after1)

      case Sum(_, operand, _*) =>
        body(code, operand)        

      case _: Sum if comprehension =>
        (prefix1, (before1, after1))

      case _: Sum =>
        val prefix2 = s"${prefix1}${indent}"

        val before2 =
          s"${prefix1}for\n" +
          s"${prefix2}_ <- IO.unit\n"
        val after2 =
          s"${prefix1}yield\n" +
          s"${prefix2}()\n"

        (prefix1, (before1 + before2, after2 + after1))

      ///////////////////////////////////////////////////////////// summation //


      // COMPOSITION ///////////////////////////////////////////////////////////

      case it @ Par(enabled, _, _, _*) =>
        val prefix2 = prefix1 + (if comprehension then "" else indent)

        val before2 =
          (if comprehension then "" else s"${prefix1}for\n") +
          s"${prefix2}_ <- IO.unit\n" +
          (if setup && enabled.nonEmpty then s"${add(enabled, prefix2)}\n" else "") +
          s"${prefix2}_ <- (\n"
        val separator =
          s"${prefix2}     ,\n"
        val after2 =
          s"${prefix2}     ).parMapN { (${"_, ".repeat(it.components.size-1)}_) => }\n" +
          (if comprehension then "" else s"${prefix1}yield\n${prefix2}()\n")

        val prefix =
          s"${prefix2}     ${indent}"

        cp = false -> prefix

        val (before3, after3) = it
          .components
          .zipWithIndex
          .foldLeft((before2, "")) { case ((before, after), (operand, i)) =>
            val (_, (before3, after3)) = body(cp -> (before, after) -> false, operand)
            (before3 + after3 + (if i < it.components.size - 1 then separator else after2), "")
          }

        prefix1 -> (before1 + before3, after3 + after1)

      case Par(_, operand, _*) =>
        body(code, operand)        

      case _: Par => ??? // not even inaction - impossible by syntax

      /////////////////////////////////////////////////////////// composition //


      // RESTRICTION & PREFIXES & (MIS)MATCH ///////////////////////////////////

      case `v`(Opd(Symbol(name))) =>
        before1 +=
          s"${prefix1}$name <- `v`\n"

        prefix1 -> (before1, after1)

      case `𝜏`(uuid, r) =>
        before1 +=
          (if setup then s"${add(Set(uuid), prefix1)}\n" else "") +
          s"${prefix1}_ <- `𝜏`(^ + \"$uuid\", ${rate(r)})\n"

        prefix1 -> (before1, after1)


      case IO(_, _, Opd(Symbol(_)), par, true) if !par.isSymbol => ??? // not binding a name - caught by parser

      case IO(_, _, ch, uuid, _) if !ch.isSymbol => ??? // not a channel name - caught by parser
 
      case IO(uuid, r, Opd(Symbol(ch)), Opd(Symbol(arg)), false) =>
        before1 +=
          (if setup then s"${add(Set(uuid), prefix1)}\n" else "") +
          s"${prefix1}_ <- $ch(^ + \"$uuid\", ${rate(r)}, $arg)\n"

        prefix1 -> (before1, after1)

      case IO(uuid, r, Opd(Symbol(ch)), Opd(Expr(expr)), false) =>
        before1 +=
          (if setup then s"${add(Set(uuid), prefix1)}\n" else "") +
          s"${prefix1}_ <- $ch(^ + \"$uuid\", ${rate(r)}, $expr)\n"

        prefix1 -> (before1, after1)


      case IO(uuid, r, Opd(Symbol(ch)), Opd(arg), false) =>
        before1 +=
          (if setup then s"${add(Set(uuid), prefix1)}\n" else "") +
          s"${prefix1}_ <- $ch(^ + \"$uuid\", ${rate(r)}, $arg)\n"

        prefix1 -> (before1, after1)

      case IO(uuid, r, Opd(Symbol(ch)), Opd(Symbol(par)), true) =>
        before1 +=
          (if setup then s"${add(Set(uuid), prefix1)}\n" else "") +
          s"${prefix1}$par <- $ch(^ + \"$uuid\", ${rate(r)})\n"

        prefix1 -> (before1, after1)


      case Match(Opd(lhs), Opd(rhs), mismatch) =>

        def === =
          (lhs, rhs) match
             case (Symbol(x), Symbol(y)) => s"$x === $y"
             case (Symbol(x), y: String) => s"$x === $y"
             case (Symbol(x), Expr(y)) => s"$x === $y"
             case (x: String, Symbol(y)) => s"$x === $y"
             case (Expr(x), Symbol(y)) => s"$x === $y"
             case (x: String, y: String) => s"$x === $y"
             case (x: String, Expr(y)) => s"$x === $y"
             case (Expr(x), y: String) => s"$x === $y"

        val prefix2 =
          s"${prefix1}     "
        val prefix3 =
          s"${prefix2}${indent}"

        before1 +=
          s"${prefix1}_ <- if ${if mismatch then "" else "!"}(${===}) then IO.unit else\n" +
          s"${prefix2}for\n" +
          s"${prefix3}_ <- IO.unit\n"

        after1 =
          s"${prefix2}yield\n" +
          s"${prefix3}()\n" +
          after1

        prefix3 -> (before1, after1)

      /////////////////////////////////// restriction & prefixes & (mis)match //


      // AGENT CALL ////////////////////////////////////////////////////////////

      case Call(Opd(Symbol(identifier)), path, params*) =>
        val args = params.map {
          case Opd(Symbol(name)) => name
          case Opd(value) =>
            value match
              case it: BigDecimal => s"BigDecimal($it)"
              case it: String => s"$it"
              case Expr(it) => s"$it"
        }

        val prefix2 = prefix1 + (if comprehension then "" else indent)

        before1 +=
          (if comprehension then "" else s"${prefix1}for\n") +
          ( if path.isEmpty
            then
              s"${prefix2}_ <- `$identifier`(${args.mkString(", ")})(\"$uuid\")\n"
            else
              s"${prefix2}_ <- ${path.mkString(".")}.`π`.`$identifier`(${args.mkString(", ")})(\"$uuid\")\n"
          )

        after1 =
          (if comprehension then "" else s"${prefix1}yield\n${prefix2}()\n") +
          after1

        prefix1 -> (before1, after1)

      case _: Call => ??? // impossible by syntax

      //////////////////////////////////////////////////////////// agent call //


      // SEQUENCE //////////////////////////////////////////////////////////////
      // followed possibly either by agent call or another process expression //

      case Seq(ast, _, it*) if it.isEmpty =>
        cp = false -> prefix1

        body(cp -> (before1, after1) -> init(ast), ast)

      case Seq(ast, enabled, it*) =>
        val prefix2 = s"${prefix1}${indent}"

        val before2 =
          s"${prefix1}for\n" +
          s"${prefix2}_ <- IO.unit\n"
        val after2 =
          s"${prefix1}yield\n" +
          s"${prefix2}()\n"

        val ((prefix3, (before3, after3)), _) = it
          .foldLeft(prefix2 -> ("", "") -> (setup && enabled.nonEmpty)) {
            case (((prefix, (before, after)), setup), pre) =>
              cp = true -> prefix
              body(cp -> (before, after) -> setup, pre) -> true
          }

        val (before, after) = (before1 + before2 + before3, after3 + after2 + after1)

        cp = true -> prefix3

        body(cp -> (before, after) -> init(ast), ast)

      ////////////////////////////////////////////////////////////// sequence //

      case it => ???


object Program:

  type Code = (((Boolean, String), (String, String)), Boolean)
  //             ^^^^^^^  ^^^^^^    ^^^^^^  ^^^^^^    ^^^^^^^
  //             |||||||  indent,   before, after,    setup
  //             whether inside a for-comprehension

  def uuid = UUID.randomUUID.toString

  def add(enabled: Actions, prefix: String): String =
    s"""${prefix}us = Set(${enabled.mkString("\"", "\", \"", "\"")})\n""" +
    s"""${prefix}_ <- %.update(us.foldLeft(_){ case (it, key) => it + ((^ + key) -> None) })"""

  def excl(enabled: Actions, prefix: String): String =
    s"""${prefix}_ <- +.update(us.foldLeft(_){ case (it, key) => it + ((^ + key) -> (us - key).map(^ + _)) })"""

  val rate: Option[Option[Any]] => String = _ match
    case Some(Some(r: BigDecimal)) => s"`@`(BigDecimal($r))"
    case Some(Some(Expr(r))) => s"`@`($r)"
    case Some(Some(Symbol(r))) => s"`@`($r)"
    case Some(_) => "`∞`"
    case _ => "null"

  val init: AST => Boolean = _ match
    case Sum(_, _, _, _*) => true
    case Sum(_, Par(_, _, _, _*), _*) => true
    case Sum(_, Par(_, Seq(_, _, _*), _*), _*) => true
    case _ => false
