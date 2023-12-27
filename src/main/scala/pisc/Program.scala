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

  private val constants = Map[AnyRef, String]()

  private def apply(value: AnyRef): String =
    if !constants.contains(value)
    then
      constants(value) = UUID.randomUUID.toString

    constants(value)

  def apply(bind: List[Bind]): String = bind
    .map { case (bind, sum) =>
      val code = (false -> indent) -> ("", "")
      defn(code)(bind, sum)
    }.mkString("\n\n")

  private def defn(code: Code)(bind: Call, sum: Sum): String =
    var ((comprehension, prefix1), (before1, after1)) = code
    val identifier = bind.identifier.asSymbol.name
    val params = bind.params.map(_.asSymbol.name).map { _.toString + ": Name" }
    before1 +=
      s"${prefix1}def `$identifier`(${params.mkString(", ")}): IO[Unit] =\n"

    val prefix =
      s"${prefix1}$indent"

    val cp = comprehension -> prefix

    val (_, (before, after)) = body(cp -> (before1, after1), sum)

    before + after

  private def body(code: Code, node: AST): (String, (String, String)) =
    //                                      ^^^^^^   ^^^^^^  ^^^^^^
    //                              [Match] prefix,  before, after
    var (cp @ (comprehension, prefix1), (before1, after1)) = code

    node match

      // SUMMATION /////////////////////////////////////////////////////////////

      case it @ Sum(operand, _, _*) =>
        val prefix2 = prefix1 + (if comprehension then "" else indent)
        val before2 =
          (if comprehension then "" else s"${prefix1}for\n") +
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

        val (prefix3, (before3, after3)) = body(cp -> ("", ""), operand)

        val (before, after) = (before1 + before2 + before3 + separator, after3 + after2 + after1)

        cp = false -> prefix3

        body(cp -> (before, after), Sum(it.choices.tail: _*))

      case Sum(operand, _*) => body(code, operand)

      case Sum(_*) => (prefix1, (before1, after1))

      ///////////////////////////////////////////////////////////// summation //


      // COMPOSITION ///////////////////////////////////////////////////////////

      case it @ Par(_, _, _*) =>
        val prefix2 = prefix1 + (if comprehension then "" else indent)
        val before2 =
          (if comprehension then "" else s"${prefix1}for\n") +
          s"${prefix2}_ <- (\n"
        val separator =
          s"${prefix2}     ,\n"
        val after2 =
          s"${prefix2}     ).parMapN { (${"_, ".repeat(it.components.size-1)}_) => }\n" +
          (if comprehension then "" else s"${prefix1}yield\n${prefix1}  ()\n")

        val prefix =
          s"${prefix2}     ${indent}"

        cp = false -> prefix

        val (before3, after3) = it
          .components
          .zipWithIndex
          .foldLeft((before2, "")) { case ((before, after), (operand, i)) =>
            val (_, (before3, after3)) = body(cp -> (before, after), operand)
            (before3 + after3 + (if i < it.components.size - 1 then separator else after2), "")
          }

        prefix1 -> (before1 + before3, after3 + after1)

      case Par(operand, _*) => body(code, operand)

      case _: Par => ??? // impossible by syntax

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

      case Pre(Opd(Symbol(ch)), Opd(value), false) =>
        val uuid = this(value)

        val arg = value match
          case it: BigDecimal => s"BigDecimal($it)"
          case it: String => it

        before1 +=
          s"${prefix1}_ <- $ch(\"$uuid\" -> $arg)\n"

        prefix1 -> (before1, after1)

      case Pre(Opd(Symbol(ch)), Opd(Symbol(par)), true) =>
        before1 +=
          s"${prefix1}$par <- $ch()\n"

        prefix1 -> (before1, after1)

      case Pre(Opd(Symbol(_)), Opd(par), true) => ??? // not binding a name

      case Pre(ch, _, _) if !ch.isSymbol => ??? // not a channel name

      case Match(Opd(lhs), Opd(rhs)) =>

        def === =
          (lhs, rhs) match
             case (Symbol(x), Symbol(y)) => s"${x}._1 == ${y}._1"
             case (Symbol(x), y: String) => s"${x}._1 == ${this(y)}"
             case (Symbol(x), y: BigDecimal) => s"${x}._1 == \"${this(y)}\""
             case (x: String, Symbol(y)) => s"\"${this(x)}\" == ${y}._1"
             case (x: BigDecimal, Symbol(y)) => s"\"${this(x)}\" == ${y}._1"
             case (x, y) => s"\"${this(x)}\" == \"${this(y)}\""

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
            val uuid = this(value)

            value match
              case it: BigDecimal => s"\"$uuid\" -> BigDecimal($it)"
              case it: String => s"\"$uuid\" -> $it"
        }

        before1 +=
          s"${prefix1}`$identifier`(${args.mkString(", ")})\n"

        prefix1 -> (before1, after1)

      case _: Call => ??? // impossible by syntax

      //////////////////////////////////////////////////////////// agent call //


      // SEQUENCE //////////////////////////////////////////////////////////////
      // followed possibly either by agent call or another process expression //

      case End(Seq(it*), ast) if it.isEmpty =>
        cp = false -> prefix1

        body(cp -> (before1, after1), ast)

      case End(Seq(it*), ast) =>
        val before2 =
          s"${prefix1}for\n" +
          s"${prefix1}${indent}_ <- IO.unit\n"
        val after2 =
          s"${prefix1}yield\n" +
          s"${prefix1}${indent}()\n"

        val prefix2 =
          s"${prefix1}${indent}"

        val (prefix3, (before3, after3)) = it
          .foldLeft(prefix2 -> ("", "")) { case ((prefix, (before, after)), action) =>
            cp = true -> prefix
            body(cp -> (before, after), action)
          }

        val (before, after) = (before1 + before2 + before3, after3 + after2 + after1)

        cp = true -> prefix3

        body(cp -> (before, after), ast)

      ////////////////////////////////////////////////////////////// sequence //

      case it => ???

object Program:

  type Code = ((Boolean, String), (String, String))
  //            ^^^^^^^  ^^^^^^    ^^^^^^  ^^^^^^
  //            |||||||  indent,   before, after needed for Match
  //            whether inside a for-comprehension
