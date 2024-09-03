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
package parser

import scala.collection.mutable.{ LinkedHashSet => Set }
import scala.util.parsing.combinator._

import Ambient._
import Calculus._

import scala.meta.{ Enumerator, Term }


class Calculus extends Ambient:

  def equation: Parser[Bind] =
    agent(true)~"="~parallel ^^ {
      case (bind, bound) ~ _ ~ (par, free)
        if (free &~ bound).nonEmpty =>
        throw EquationFreeNamesException(bind.identifier, free &~ bound)
      case (bind, _) ~ _ ~ (par, _) =>
        bind -> flatten(par)
    }

  def parallel: Parser[(`|`, Names)] =
    rep1sep(sequential, "|") ^^ { ss =>
      `|`(ss.map(_._1)*) -> ss.map(_._2).reduce(_ ++ _)
    }

  def sequential: Parser[(`.`, Names)] =
    prefixes ~ opt( leaf | "("~>parallel<~")" ) ^^ {
      case pre ~ Some((end, free)) =>
        `.`(end, pre._1*) -> (pre._2._2 ++ (free &~ pre._2._1))
      case pre ~ _ =>
        `.`(∅, pre._1*) -> pre._2._2 // void
    }

  def leaf: Parser[(`-`, Names)] = agent() |
    "!" ~> opt( "."~> "("~>name<~")" <~"." ) ~ parallel ^^ { // [guarded] replication
      case Some((it, bound)) ~ (par, free) =>
        `!`(Some(it), par) -> (free &~ bound)
      case None ~ (par, free) =>
        `!`(None, par) -> free
    } |
    name ~ ("["~>parallel<~"]") ^^ { // ambient
      case (amb, name) ~ (par, free) =>
        `[]`(amb, par) -> (name ++ free)
    } |
    ("<"~>caps<~">") ~ opt( expression ) ^^ { // output action
      case _ ~ Some((Left(enums), _)) =>
        throw TermParsingException(enums)
      case (path, free) ~ Some((Right(it), free2)) =>
        `<>`(Some(it), path*) -> (free ++ free2)
      case (path, free) ~ _ =>
        `<>`(None, path*) -> free
    } |
    "go" ~> name ~ ("."~> parallel) ^^ { // objective move
      case (amb, name) ~ (par, free) =>
        `go.`(amb, par) -> (name ++ free)
    }

  def prefixes: Parser[(List[Pre], (Names, Names))] =
    rep(prefix) ^^ { ps =>
      val bound = ps.map(_._2._1)
      val free = ps.map(_._2._2)
        .zipWithIndex
        .foldLeft(Names()) { case (r, (ns, i)) =>
          ns.foldLeft(r) {
            case (r, n)
              if {
                val j = bound.indexWhere(_.contains(n))
                j < 0 || i <= j
              } => r + n
            case (r, _) => r
          }
        }
      ps.map(_._1) -> (if bound.nonEmpty then bound.reduce(_ ++ _) else Names(), free)
    }

  def prefix: Parser[(Pre, (Names, Names))] =
    "ν"~>"("~>rep1sep(name, ",")<~")" ^^ { ns => // restriction
      ν(ns.map(_._1)*) -> (ns.map(_._2).reduce(_ ++ _), Names())
    } |
    "τ" ~> opt( expression ) <~ "." ^^ { // silent transition
      case Some((it, free)) =>
        τ(Some(it)) -> (Names(), free)
      case _ =>
        τ(None) -> (Names(), Names())
    } |
    caps <~ "." ^^ { // capability action
      case (path, free) =>
      `..`(path*) -> (Names(), free)
    } |
    ("("~>name<~")") ~ opt( expression ) <~ "." ^^ { // input action
      case _ ~ Some((Left(enums), _)) =>
        throw TermParsingException(enums)
      case (name, bound) ~ Some((Right(it), free2)) =>
        `()`(Some(it), name) -> (bound, free2)
      case (name, bound) ~ _ =>
        `()`(None, name) -> (bound, Names())
    }

  def agent(binding: Boolean = false): Parser[(`(*)`, Names)] =
    qual ~ IDENT ~ opt( "("~>repsep(name, ",")<~")" ) ^^ {
      case qual ~ id ~ _ if binding && qual.nonEmpty =>
        throw EquationQualifiedException(id, qual)
      case qual ~ id ~ Some(params) =>
        `(*)`(id, qual, params.map(_._1)*) -> params.map(_._2).foldLeft(Set.empty)(_ ++ _)
      case qual ~ id ~ _ =>
        `(*)`(id, qual) -> Names()
    }

  /**
   * Agent identifiers start with upper case.
   * @return
   */
  def IDENT: Parser[String] =
      "" ~> // handle whitespace
      rep1(acceptIf(Character.isUpperCase)("agent identifier expected but '" + _ + "' found"),
          elem("agent identifier part", { (ch: Char) => Character.isJavaIdentifierPart(ch) || ch == '\'' || ch == '"' })) ^^ (_.mkString)

  /**
   * Qualified identifiers to agents in other packages.
   * @return
   */
  def qual: Parser[List[String]] =
    rep("""[{][^}]*[}]""".r) ^^ { _.map(_.stripPrefix("{").stripSuffix("}")) }


object Calculus:

  type Bind = (`(*)`, `|`)

  sealed trait AST extends Any

  case class `|`(components: `.`*) extends AST

  object ∅ extends `|`():
    override def canEqual(that: Any): Boolean =
      that.isInstanceOf[`|`]
    override def equals(any: Any): Boolean = any match
      case that: `|` => that.components.size == 0
      case _ => false

  case class `.`(end: `&`, prefixes: Pre*) extends AST

  sealed trait Pre extends Any

  case class ν(names: String*) extends AnyVal with Pre // forcibly

  case class τ(code: Option[Either[List[Enumerator], Term]]) extends AnyVal with Pre

  case class `..`(path: Ambient.AST*) extends Pre

  case class `()`(code: Option[Term], name: String) extends Pre

  case class `<>`(code: Option[Term], path: Ambient.AST*) extends AST

  case class `!`(guard: Option[String], par: `|`) extends AST

  case class `[]`(amb: String, par: `|`) extends AST

  case class `go.`(amb: String, par: `|`) extends AST

  case class `(*)`(identifier: String,
                   qual: List[String],
                   params: String*) extends AST


  // exceptions

  import Expression.ParsingException

  class EquationParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class EquationQualifiedException(id: String, qual: List[String])
      extends EquationParsingException(s"A qualified package ${qual.mkString(".")} is present in the left hand side of $id")

  case class EquationFreeNamesException(id: String, free: Names)
      extends EquationParsingException(s"The free names (${free.mkString(", ")}) in the right hand side are not formal parameters of the left hand side of $id")

  import scala.meta.Enumerator

  case class TermParsingException(enums: List[Enumerator])
      extends ParsingException(s"The embedded Scalameta should be a Term, not Enumerator `$enums'")


  // functions

  def flatten[T <: AST](ast: T): T =

    inline given Conversion[AST, T] = _.asInstanceOf[T]

    ast match

      case `∅` => ∅

      case `|`(`.`(par: `|`), it*) =>
        val lhs = flatten(par)
        val rhs = flatten(`|`(it*))
        `|`((lhs.components ++ rhs.components).filterNot(∅ == `|`(_))*)

      case `|`(seq, it*) =>
        val lhs = `|`(flatten(seq))
        val rhs = flatten(`|`(it*))
        `|`((lhs.components ++ rhs.components).filterNot(∅ == `|`(_))*)

      case `.`(`|`(`.`(end, ps*)), it*) =>
        flatten(`.`(end, (it ++ ps)*))

      case `.`(end, it*) =>
        `.`(flatten(end), it*)

      case `!`(None, par) =>
        flatten(par) match
          case `|`(`.`(end: `!`)) => end
          case it => `!`(None, it)

      case `!`(guard, par) =>
        `!`(guard, flatten(par))

      case `[]`(amb, par) =>
        `[]`(amb, flatten(par))

      case `go.`(amb, par) =>
        `go.`(amb, flatten(par))

      case _ => ast
