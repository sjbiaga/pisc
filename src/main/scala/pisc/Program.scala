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

import scala.collection.mutable.{ HashMap => Map }

import Calculus.{ rate => _, _ }
import Program._


final class Program(indent: String = "  "):

  def apply(bind: List[Bind]): List[String] =
    val mutex = Map[String, Sum]()
    bind
      .map { (bind, sum) =>
        val code = (false -> indent) -> ("", "") -> (null, mutex)
        defn(code)(bind, sum)
      } :+ {
        mutex.map { (method, sum) =>
          defn(indent, method, sum)
        }.mkString("\n\n")
      }


  private def defn(code: Code)(bind: Call, sum: Sum): String =
    var (((comprehension, prefix1), (before1, after1)), (_, mutex)) = code
    val identifier = bind.identifier.asSymbol.name
    val params = bind.params.map(_.asSymbol.name).map { _.toString + ": `()`" }

    val prefix2 =
      s"${prefix1}     ${" ".repeat(identifier.length)} "

    before1 +=
      s"${prefix1}def `$identifier`(${params.mkString(", ")})(^ : String)\n" +
      s"${prefix2}(using % : %, / : /, + : +, - : -): IO[Unit] =\n"

    val prefix =
      s"${prefix1}$indent"

    val cp = comprehension -> prefix

    val (_, (before, after)) = body(cp -> (before1, after1) -> (Actions(), mutex), sum)

    before + after


  private def body(code: Code, node: AST): (String, (String, String)) =
    //                                      ^^^^^^   ^^^^^^  ^^^^^^
    //                                      prefix,  before, after
    var ((cp @ (comprehension, prefix1), (before1, after1)), (implied, mutex)) = code

    node match

      // SUMMATION /////////////////////////////////////////////////////////////

      case it @ Sum(enabled_, _, _, _*) =>
        val enabled = enabled_ -- implied

        val method = if enabled.nonEmpty then uuid else null
        if method ne null then mutex(method) = it

        val prefix2 = prefix1 + (if comprehension then "" else indent)

        val before2 =
          (if comprehension then "" else s"${prefix1}for\n") +
          s"${prefix2}_ <- IO.unit\n" +
          (if enabled.nonEmpty then s"${add(enabled, prefix2)}\n" else "") +
          (if enabled.nonEmpty then s"${excl(enabled, prefix2, method)}\n" else "") +
          s"${prefix2}_ <- (\n"
        val separator =
          s"${prefix2}     ,\n"
        val after2 =
          s"${prefix2}     ).parMapN { (${"_, ".repeat(it.choices.size-1)}_) => }\n" +
          (if comprehension then "" else s"${prefix1}yield\n${prefix2}()\n")

        val prefix =
          s"${prefix2}     ${indent}"

        cp = false -> prefix

        if enabled.nonEmpty then implied = enabled

        val (before3, after3) = it
          .choices
          .zipWithIndex
          .foldLeft((before2, "")) { case ((before, after), (operand, i)) =>
            val (_, (before3, after3)) = body(cp -> (before, after) -> (implied, mutex), operand)
            (before3 + after3 + (if i < it.choices.size - 1 then separator else after2), "")
          }

        prefix1 -> (before1 + before3, after3 + after1)

      case Sum(_, operand, _*) =>
        body(code, operand)

      case _: Sum => ???

      ///////////////////////////////////////////////////////////// summation //


      // COMPOSITION ///////////////////////////////////////////////////////////

      case it @ Par(enabled_, _, _, _*) =>
        val enabled = enabled_ -- implied

        val prefix2 = prefix1 + (if comprehension then "" else indent)

        val before2 =
          (if comprehension then "" else s"${prefix1}for\n") +
          s"${prefix2}_ <- IO.unit\n" +
          (if enabled.nonEmpty then s"${add(enabled, prefix2)}\n" else "") +
          s"${prefix2}_ <- (\n"
        val separator =
          s"${prefix2}     ,\n"
        val after2 =
          s"${prefix2}     ).parMapN { (${"_, ".repeat(it.components.size-1)}_) => }\n" +
          (if comprehension then "" else s"${prefix1}yield\n${prefix2}()\n")

        val prefix =
          s"${prefix2}     ${indent}"

        cp = false -> prefix

        if enabled.nonEmpty then implied = enabled

        val (before3, after3) = it
          .components
          .zipWithIndex
          .foldLeft((before2, "")) { case ((before, after), (operand, i)) =>
            val (_, (before3, after3)) = body(cp -> (before, after) -> (implied, mutex), operand)
            (before3 + after3 + (if i < it.components.size - 1 then separator else after2), "")
          }

        prefix1 -> (before1 + before3, after3 + after1)

      case Par(_, operand, _*) =>
        body(code, operand)

      case _: Par if comprehension =>
        (prefix1, (before1, after1))

      case _: Par =>
        val prefix2 = s"${prefix1}${indent}"

        val before2 =
          s"${prefix1}for\n" +
          s"${prefix2}_ <- IO.unit\n"
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

      case `𝜏`(uuid, r) =>
        val enabled = Set(uuid) -- implied

        before1 +=
          (if enabled.nonEmpty then s"${add(enabled, prefix1)}\n" else "") +
          s"${prefix1}_ <- `𝜏`(^ + \"$uuid\", ${rate(r)})\n"

        prefix1 -> (before1, after1)


      case IO(_, _, Opd(Symbol(_)), par, true) if !par.isSymbol => ??? // not binding a name - caught by parser

      case IO(_, _, ch, uuid, _) if !ch.isSymbol => ??? // not a channel name - caught by parser

      case IO(uuid, r, Opd(Symbol(ch)), Opd(Symbol(arg)), false) =>
        val enabled = Set(uuid) -- implied

        before1 +=
          (if enabled.nonEmpty then s"${add(enabled, prefix1)}\n" else "") +
          s"${prefix1}_ <- $ch(^ + \"$uuid\", ${rate(r)}, $arg)\n"

        prefix1 -> (before1, after1)

      case IO(uuid, r, Opd(Symbol(ch)), Opd(Expr(expr)), false) =>
        val enabled = Set(uuid) -- implied

        before1 +=
          (if enabled.nonEmpty then s"${add(enabled, prefix1)}\n" else "") +
          s"${prefix1}_ <- $ch(^ + \"$uuid\", ${rate(r)}, $expr)\n"

        prefix1 -> (before1, after1)

      case IO(uuid, r, Opd(Symbol(ch)), Opd(arg), false) =>
        val enabled = Set(uuid) -- implied

        before1 +=
          (if enabled.nonEmpty then s"${add(enabled, prefix1)}\n" else "") +
          s"${prefix1}_ <- $ch(^ + \"$uuid\", ${rate(r)}, $arg)\n"

        prefix1 -> (before1, after1)

      case IO(uuid, r, Opd(Symbol(ch)), Opd(Symbol(par)), true) =>
        val enabled = Set(uuid) -- implied

        before1 +=
          (if enabled.nonEmpty then s"${add(enabled, prefix1)}\n" else "") +
          s"${prefix1}$par <- $ch(^ + \"$uuid\", ${rate(r)})\n"

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

        val (_, (before3, after3)) = body(cp -> ("", "") -> (implied, mutex), t)

        body(cp -> (before1 + before2 + before3 + after3 + separator, after2 + after1) -> (implied, mutex), f)


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

        body(cp -> (before1, after1) -> (implied, mutex), sum)

      ////// restriction | prefixes | (mis)match | if then else | replication //


      // agent call ////////////////////////////////////////////////////////////

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
              s"${prefix2}_ <- `$identifier`(${args.mkString(", ")})(`π-uuid`)\n"
            else
              s"${prefix2}_ <- ${path.mkString(".")}.`π`.`$identifier`(${args.mkString(", ")})(`π-uuid`)\n"
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

        body(cp -> (before1, after1) -> (implied, mutex), ast)

      case Seq(ast, _, it*) =>
        val prefix2 = prefix1 + (if comprehension then "" else indent)

        val before2 =
          (if comprehension then "" else s"${prefix1}for\n") +
          s"${prefix2}_ <- IO.unit\n"
        val after2 =
          (if comprehension then "" else s"${prefix1}yield\n${prefix2}()\n")

        var first = true

        val (prefix3, (before3, after3)) = it
          .foldLeft(prefix2 -> ("", "")) {
            case ((prefix, (before, after)), pre) =>
              cp = true -> prefix
              val isAct = pre.isInstanceOf[Act]
              if first && isAct
              then
                first = false
              else if (isAct || !pre.isInstanceOf[`v`]) && implied.nonEmpty
              then
                implied = Actions()
              body(cp -> (before, after) -> (if isAct then implied else Actions(), mutex), pre)
          }

        val (before, after) = (before1 + before2 + before3, after3 + after2 + after1)

        cp = true -> prefix3

        body(cp -> (before, after) -> (Actions(), mutex), ast)

      ////////////////////////////////////////////////////////////// sequence //

      case it => ???


  private def defn(indent: String, method: String, sum: Sum): String =
    val prefix1 = s"$indent"
    val prefix2 = s"$prefix1$indent"

    val exclude = Map[String, Actions]()
    val include = Map[String, Actions]()

    sum.enabled.foreach(exclude(_) = Actions())
    sum.enabled.foreach(include(_) = Actions())

    lazy val rec: AST => Actions = {

      case Sum(enabled, ps*) =>
        val ls = ps.map(rec)

        ls.zipWithIndex.foreach { (it, i) =>
          val ks = (ls.take(i) ++ ls.drop(i+1)).reduce(_ ++ _)
          it.foreach { k => include(k) = include(k) ++ ks }
        }

        enabled

      case Par(enabled, ss*) =>
        val ls = ss.map(rec)

        ls.zipWithIndex.foreach { (it, i) =>
          val ks = (ls.take(i) ++ ls.drop(i+1)).foldLeft(Actions())(_ ++ _)
          it.foreach { k => exclude(k) = exclude(k) ++ ks }
        }

        enabled

      case Seq(ast, _, ps*) =>
        val enabled = Actions(ps: _*)

        if enabled.isEmpty
        then
          rec(ast)
        else
          enabled

      case _ =>
        Actions()

    }

    rec(sum)

    val prefix3 =
      s"${prefix2}            "

    s"${prefix1}def `$method`(en: Set[String], key: String)(^ : String): Set[String] =\n" +
    s"${prefix2}val exc = ( if false then Set.empty\n" +
    exclude.foldLeft("") { case (r, (key, enabled)) => r +
      s"""${prefix3}else if key == \"$key\" then Set(${enabled.mkString("\"", "\", \"", "\"")}).map(^ + _)\n"""
    } +
    s"${prefix3}else Set.empty )\n" +
    s"${prefix2}val inc = ( if false then Set.empty\n" +
    include.foldLeft("") { case (r, (key, enabled)) => r +
      s"""${prefix3}else if key == \"$key\" then Set(${enabled.mkString("\"", "\", \"", "\"")}).map(^ + _)\n"""
    } +
    s"${prefix3}else Set.empty )\n" +
    s"${prefix2}((en - key).map(^ + _) -- exc) ++ inc\n"


object Program:

  type Code = (((Boolean, String), (String, String)), (Actions, Map[String, Sum]))
  //             ^^^^^^^  ^^^^^^    ^^^^^^  ^^^^^^    ^^^^^^^^  ^^^^^^^^^^^^^^^^
  //             for-c.,  indent,   before, after,    implied,  mutual exclusive

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

  def add(enabled: Actions, prefix: String): String =
    s"""${prefix}en = Set(${enabled.mkString("\"", "\", \"", "\"")})\n""" +
    s"""${prefix}_ <- %.update(en.foldLeft(_){ case (it, key) => it + ((^ + key) -> None) })"""

  def excl(enabled: Actions, prefix: String, method: String): String =
    s"""${prefix}_ <- +.update(en.foldLeft(_){ case (it, key) => it + ((^ + key) -> `$method`(en, key)(^)) })"""

  val rate: Option[Option[Any]] => String = _ match
    case Some(Some(r: BigDecimal)) => s"`@`(BigDecimal($r))"
    case Some(Some(Expr(r))) => s"`@`($r)"
    case Some(Some(Symbol(r))) => s"`@`($r)"
    case Some(_) => "`∞`"
    case _ => "null"
