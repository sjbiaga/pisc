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

package pisc

import java.util.UUID

import Calculus._
import Program._


final class Program(indent: String = "  "):

  def apply(bind: List[Bind]): String = bind
    .map { case (bind, sum) =>
      val code = (false -> indent) -> ("", "") -> None
      defn(code)(bind, sum)
    }.mkString("\n\n")


  private def defn(code: Code)(bind: Call, sum: Sum): String =
    var (((comprehension, prefix1), (before1, after1)), _) = code
    val identifier = bind.identifier.asSymbol.name
    val params = bind.params.map(_.asSymbol.name).map { _.toString + ": `()`" }

    before1 +=
      s"${prefix1}def `$identifier`(${params.mkString(", ")}): IO[Unit] =\n"

    val prefix =
      s"${prefix1}$indent"

    val cp = comprehension -> prefix

    val (_, (before, after)) = body(cp -> (before1, after1) -> None, sum)

    before + after


  private def body(code: Code, node: AST): (String, (String, String)) =
    //                                      ^^^^^^   ^^^^^^  ^^^^^^
    //                                      prefix,  before, after
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

      case _: Sum => ??? // not even inaction - impossible by syntax

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

      case _: Par if comprehension =>
        (prefix1, (before1, after1))

      case _: Par =>
        val prefix2 = s"${prefix1}${indent}"

        val before2 =
          s"${prefix1}for\n" +
          s"${prefix2}_ <- IO.unit\n" +
          semaphore.map(s"${prefix2}_ <- `" + _ + "`.acquire\n").getOrElse("")
        val after2 =
          s"${prefix1}yield\n" +
          s"${prefix2}()\n"

        (prefix1, (before1 + before2, after2 + after1))

      /////////////////////////////////////////////////////////// composition //


      // RESTRICTION | PREFIXES | (MIS)MATCH | IF THEN ELSE | REPLICATION //////

      case `v`(Opd(Symbol(name))) =>
        before1 +=
          s"${prefix1}$name <- `v`\n"

        prefix1 -> (before1, after1)

      case `𝜏` =>
        before1 +=
          s"${prefix1}_ <- `𝜏`\n"

        prefix1 -> (before1, after1)


      case IO(Opd(Symbol(_)), par, true) if !par.isSymbol => ??? // not binding a name - caught by parser

      case IO(ch,  _, _) if !ch.isSymbol => ??? // not a channel name - caught by parser

      case IO(Opd(Symbol(ch)), Opd(Symbol(arg)), false) =>
        before1 +=
          s"${prefix1}_ <- $ch($arg)\n"

        prefix1 -> (before1, after1)

      case IO(Opd(Symbol(ch)), Opd(Expr(expr)), false) =>
        before1 +=
          s"${prefix1}_ <- $ch($expr)\n"

        prefix1 -> (before1, after1)

      case IO(Opd(Symbol(ch)), Opd(arg), false) =>
        before1 +=
          s"${prefix1}_ <- $ch($arg)\n"

        prefix1 -> (before1, after1)

      case IO(Opd(Symbol(ch)), Opd(Symbol(par)), true) =>
        before1 +=
          s"${prefix1}$par <- $ch()\n"

        prefix1 -> (before1, after1)


      case Match(Opd(lhs), Opd(rhs), mismatch) =>

        val prefix2 =
          s"${prefix1}     "
        val prefix3 =
          s"${prefix2}${indent}"

        before1 +=
          s"${prefix1}_ <- if ${if mismatch then "" else "!"}(${===(lhs -> rhs)}) then IO.unit else\n" +
          s"${prefix2}for\n" +
          s"${prefix3}_ <- IO.unit\n"

        after1 =
          s"${prefix2}yield\n" +
          s"${prefix3}()\n" +
          after1

        prefix3 -> (before1, after1)


      case `?:`(((Opd(lhs), Opd(rhs)), mismatch), t, f) =>

        val prefix2 =
          s"${prefix1}       "
        val prefix3 =
          s"${prefix2}${indent}"

        val before2 =
          s"${prefix1}_ <- ( if ${if mismatch then "!" else ""}(${===(lhs -> rhs)})\n" +
          s"${prefix2}then\n"
        val separator =
          s"${prefix2}else\n"
        val after2 =
          s"${prefix1}     )\n"

        cp = false -> prefix3

        val (_, (before3, after3)) = body(cp -> ("", "") -> None, t)

        body(cp -> (before1 + before2 + before3 + after3 + separator, after2 + after1) -> None, f)


      case `!`(sum) =>

        val name = uuid

        val prefix2 = prefix1 + (if comprehension then "" else indent)

        val prefix3 =
          s"${prefix2}${indent}"
        val prefix4 =
          s"${prefix3}${indent}"
        val prefix5 =
          s"${prefix4}${indent}"

        before1 +=
          (if comprehension then "" else s"${prefix1}for\n") +
          s"${prefix2}pi <- IO {\n" +
          s"${prefix3}lazy val `$name`: IO[Unit] =\n" +
          s"${prefix4}for\n"

        after1 =
          s"${prefix5}_ <- `$name`\n" +
          s"${prefix4}yield\n" +
          s"${prefix5}()\n" +
          s"${prefix3}`$name`\n" +
          s"${prefix2}}\n" +
          s"${prefix2}_ <- pi\n" +
          (if comprehension then "" else s"${prefix1}yield\n${prefix2}()\n") +
          after1

        cp = true -> prefix5

        body(cp -> (before1, after1) -> None, sum)

      ////// restriction | prefixes | (mis)match | if then else | replication //


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
          semaphore.map(s"${prefix2}_ <- `" + _ + "`.acquire\n").getOrElse("") +
          ( if path.isEmpty
            then
              s"${prefix2}_ <- `$identifier`(${args.mkString(", ")})\n"
            else
              s"${prefix2}_ <- ${path.mkString(".")}.`π`.`$identifier`(${args.mkString(", ")})\n"
          )

        after1 =
          (if comprehension then "" else s"${prefix1}yield\n${prefix2}()\n") +
          after1

        prefix1 -> (before1, after1)

      case _: Call => ??? // impossible by syntax

      //////////////////////////////////////////////////////////// agent call //


      // SEQUENCE //////////////////////////////////////////////////////////////
      // followed possibly either by agent call or another process expression //

      case Seq(ast, it*) if it.isEmpty =>
        cp = false -> prefix1

        body(cp -> (before1, after1) -> semaphore, ast)

      case Seq(ast, it*) =>
        val prefix2 = prefix1 + (if comprehension then "" else indent)

        val before2 =
          (if comprehension then "" else s"${prefix1}for\n") +
          s"${prefix2}_ <- IO.unit\n" +
          semaphore.map(s"${prefix2}_ <- `" + _ + "`.acquire\n").getOrElse("")
        val after2 =
          (if comprehension then "" else s"${prefix1}yield\n${prefix2}()\n")

        val (prefix3, (before3, after3)) = it
          .foldLeft(prefix2 -> ("", "")) { case ((prefix, (before, after)), pre) =>
            cp = true -> prefix
            body(cp -> (before, after) -> None, pre)
          }

        val (before, after) = (before1 + before2 + before3, after3 + after2 + after1)

        cp = true -> prefix3

        body(cp -> (before, after) -> None, ast)

      ////////////////////////////////////////////////////////////// sequence //

      case it => ???


object Program:

  type Code = (((Boolean, String), (String, String)), Option[String])
  //             ^^^^^^^  ^^^^^^    ^^^^^^  ^^^^^^    ^^^^^^^^^^^^^
  //             for-c.,  indent,   before, after,    semaphore

  def uuid = UUID.randomUUID.toString

  def === : ((AnyRef, AnyRef)) => String = {
     case (Symbol(x), Symbol(y)) => s"$x === $y"
     case (Symbol(x), y: String) => s"$x === $y"
     case (Symbol(x), Expr(y)) => s"$x === $y"
     case (x: String, Symbol(y)) => s"$x === $y"
     case (Expr(x), Symbol(y)) => s"$x === $y"
     case (x: String, y: String) => s"$x === $y"
     case (x: String, Expr(y)) => s"$x === $y"
     case (Expr(x), y: String) => s"$x === $y"
  }
