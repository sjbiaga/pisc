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

import Calculus._
import Program._


final class Program(indent: String = "  "):

  def uuid = UUID.randomUUID

  def apply(bind: List[Bind]): String = bind
    .map { case (bind, sum) =>
      val code = (false -> indent) -> ("", "") -> None
      defn(code)(bind, sum)
    }.mkString("\n\n")

  private def defn(code: Code)(bind: Call, sum: Sum): String =
    var (((comprehension, prefix1), (before1, after1)), _) = code
    val identifier = bind.identifier.asSymbol.name
    val params = bind.params.map(_.asSymbol.name).map { _.toString + ": Name" }
    before1 +=
      s"${prefix1}def `$identifier`(${params.mkString(", ")}): IO[Unit] =\n"

    val prefix =
      s"${prefix1}$indent"

    val cp = comprehension -> prefix

    val (_, (before, after)) = body(cp -> (before1, after1) -> None, sum)

    before + after

  private def body(code: Code, node: AST): (String, (String, String)) =
    //                                      ^^^^^^   ^^^^^^  ^^^^^^
    //                              [Match] prefix,  before, after
    var ((cp @ (comprehension, prefix1), (before1, after1)), semaphore) = code

    node match

      // SUMMATION /////////////////////////////////////////////////////////////

      case it @ Sum(operand, _, _*) =>
        val prefix2 = prefix1 + (if comprehension then "" else indent)

        val sem = Some(uuid)

        val before2 =
          (if comprehension then "" else s"${prefix1}for\n") +
          semaphore.map(s"${prefix2}_ <- `" + _ + "`.acquire\n").getOrElse("") +
          s"${prefix2}`${sem.get}` <- Semaphore[IO](1)\n" +
          s"${prefix2}_ <- IO.race\n" +
          s"${prefix2}     (\n"
        val separator =
          s"${prefix2}     ,\n"
        val after2 =
          s"${prefix2}     )\n" +
          (if comprehension then "" else s"${prefix1}yield\n${prefix1}  ()\n")

        val prefix =
          s"${prefix2}     ${indent}"

        cp = false -> prefix

        val (_, (before3, after3)) = body(cp -> ("", "") -> sem, operand)

        before1 += before2 + before3 + after3 + separator

        val (_, (before4, after4)) = body(cp -> (before1, "") -> sem, Sum(it.choices.tail: _*))

        (prefix1 -> (before4, after4 + after2 + after1))

      case Sum(operand, _*) =>
        body(code, operand)

      case Sum(_*) if comprehension =>
        (prefix1, (before1, after1))

      case Sum(_*) =>
        val prefix2 = s"${prefix1}${indent}"

        val before2 =
          s"${prefix1}for\n" +
          s"${prefix2}_ <- IO.unit\n" +
          semaphore.map(s"${prefix2}_ <- `" + _ + "`.acquire\n").getOrElse("")
        val after2 =
          s"${prefix1}yield\n" +
          s"${prefix2}()\n"

        (prefix1, (before1 + before2, after2 + after1))

      ///////////////////////////////////////////////////////////// summation //


      // COMPOSITION ///////////////////////////////////////////////////////////

      case it @ Par(_, _, _*) =>
        val prefix2 = prefix1 + (if comprehension then "" else indent)

        val before2 =
          (if comprehension then "" else s"${prefix1}for\n") +
          semaphore.map(s"${prefix2}_ <- `" + _ + "`.acquire\n").getOrElse("") +
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
            val (_, (before3, after3)) = body(cp -> (before, after) -> None, operand)
            (before3 + after3 + (if i < it.components.size - 1 then separator else after2), "")
          }

        prefix1 -> (before1 + before3, after3 + after1)

      case Par(operand, _*) =>
        body(code, operand)

      case _: Par => ??? // not even inaction - impossible by syntax

      /////////////////////////////////////////////////////////// composition //


      // PREFIXES & MATCH //////////////////////////////////////////////////////

      case `v`(Opd(Symbol(name))) =>
        before1 +=
          s"${prefix1}$name <- `v`\n"

        prefix1 -> (before1, after1)

      case `𝜏` =>
        before1 +=
          s"${prefix1}_ <- `𝜏`\n"

        prefix1 -> (before1, after1)


      case Pre(Opd(Symbol(ch)), Opd(Symbol(arg)), false) =>
        before1 +=
          s"${prefix1}_ <- $ch($arg)\n"

        prefix1 -> (before1, after1)

      case Pre(Opd(Symbol(ch)), Opd(Expr(expr)), false) =>
        before1 +=
          s"${prefix1}_ <- $ch(\"\" -> ${strip(expr)})\n"

        prefix1 -> (before1, after1)


      case Pre(Opd(Symbol(ch)), Opd(value), false) =>
        val arg = value match
          case it: BigDecimal => s"BigDecimal($it)"
          case it: String => it

        before1 +=
          s"${prefix1}_ <- $ch(\"\" -> $arg)\n"

        prefix1 -> (before1, after1)

      case Pre(Opd(Symbol(ch)), Opd(Symbol(par)), true) =>
        before1 +=
          s"${prefix1}$par <- $ch()\n"

        prefix1 -> (before1, after1)

      case Pre(Opd(Symbol(_)), par, true) if !par.isSymbol => ??? // not binding a name - caught by parser

      case Pre(ch, _, _) if !ch.isSymbol => ??? // not a channel name - caught by parser

      case Match(Opd(lhs), Opd(rhs), mismatch) =>

        def === =
          val op = if mismatch then "!=" else "=="
          (lhs, rhs) match
             case (Symbol(x), Symbol(y)) => s"${x}._1 $op ${y}._1"
             case (Symbol(x), y: String) => s"${x}._2 $op $y"
             case (Symbol(x), y: BigDecimal) => s"${x}._2 $op $y"
             case (Symbol(x), Expr(y)) => s"${x}._2 $op ${strip(y)}"
             case (x: String, Symbol(y)) => s"$x $op ${y}._2"
             case (x: BigDecimal, Symbol(y)) => s"$x $op ${y}._2"
             case (Expr(x), Symbol(y)) => s"${strip(x)} $op ${y}._2"
             case (x, y) => s"$x $op $y"

        val prefix2 =
          s"${prefix1}     "
        val prefix3 =
          s"${prefix2}${indent}"

        before1 +=
          s"${prefix1}_ <- if !(${===}) then IO.unit else\n" +
          s"${prefix2}for\n" +
          s"${prefix3}_ <- IO.unit\n"

        after1 =
          s"${prefix2}yield\n" +
          s"${prefix3}()\n" +
          after1

        prefix3 -> (before1, after1)

      ////////////////////////////////////////////////////// prefixes & match //


      // AGENT CALL ////////////////////////////////////////////////////////////

      case Call(Opd(Symbol(identifier)), params*) =>
        val args = params.map {
          case Opd(Symbol(name)) => name
          case Opd(value) =>
            value match
              case it: BigDecimal => s"\"\" -> BigDecimal($it)"
              case it: String => s"\"\" -> $it"
              case Expr(it) =>
                var expr = it.stripPrefix("'").stripSuffix("'")
                if expr == it
                then expr = it.stripPrefix("`").stripSuffix("`")

                s"\"\" -> $expr"
        }

        val prefix2 = prefix1 + (if comprehension then "" else indent)

        before1 +=
          (if comprehension then "" else s"${prefix1}for\n") +
          semaphore.map(s"${prefix2}_ <- `" + _ + "`.acquire\n").getOrElse("") +
          s"${prefix2}_ <- `$identifier`(${args.mkString(", ")})\n"

        after1 =
          (if comprehension then "" else s"${prefix1}yield\n${prefix2}()\n") +
          after1

        prefix1 -> (before1, after1)

      case _: Call => ??? // impossible by syntax

      //////////////////////////////////////////////////////////// agent call //


      // SEQUENCE //////////////////////////////////////////////////////////////
      // followed possibly either by agent call or another process expression //

      case End(Seq(it*), ast) if it.isEmpty =>
        cp = false -> prefix1

        body(cp -> (before1, after1) -> semaphore, ast)

      case End(Seq(it*), ast) =>
        val prefix2 = s"${prefix1}${indent}"

        val before2 =
          s"${prefix1}for\n" +
          s"${prefix2}_ <- IO.unit\n" +
          semaphore.map(s"${prefix2}_ <- `" + _ + "`.acquire\n").getOrElse("")
        val after2 =
          s"${prefix1}yield\n" +
          s"${prefix2}()\n"

        val (prefix3, (before3, after3)) = it
          .foldLeft(prefix2 -> ("", "")) { case ((prefix, (before, after)), action) =>
            cp = true -> prefix
            body(cp -> (before, after) -> None, action)
          }

        val (before, after) = (before1 + before2 + before3, after3 + after2 + after1)

        cp = true -> prefix3

        body(cp -> (before, after) -> None, ast)

      ////////////////////////////////////////////////////////////// sequence //

      case it => ???

object Program:

  def strip(expr_ : String): String =
    var expr = expr_.stripPrefix("'").stripSuffix("'")
    if expr == expr_
    then expr = expr_.stripPrefix("`").stripSuffix("`")
    expr


  type Code = (((Boolean, String), (String, String)), Option[UUID])
  //             ^^^^^^^  ^^^^^^    ^^^^^^  ^^^^^^    ^^^^^^^^^^^^
  //             |||||||  indent,   before, after,    semaphore
  //             whether inside a for-comprehension
