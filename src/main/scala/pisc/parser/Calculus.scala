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
package parser

import scala.collection.mutable.{ LinkedHashSet => Set }

import scala.util.parsing.combinator._

import PolyadicPi.{ Names, PrefixChannelsParsingException }
import Calculus._

import scala.meta.{ Enumerator, Term }


class Calculus extends PolyadicPi:

  def equation: Parser[Bind] =
    agent(true)~"="~choice ^^ {
      case (bind, bound) ~ _ ~ (sum, free)
        if (free &~ bound).nonEmpty =>
        throw EquationFreeNamesException(bind.identifier.asSymbol.name, free &~ bound)
      case (`(*)`(λ(Symbol("Main")), _, params*), _) ~ _ ~ _ if params.nonEmpty =>
        throw MainParsingException(params.map(_.asSymbol.name)*)
      case (bind, _) ~ _ ~ (sum, _) =>
        bind -> flatten(sum)
    }

  def choice: Parser[(`+`, Names)] =
    rep1sep(parallel, "+") ^^ { ps =>
      `+`(ps.map(_._1)*) -> ps.map(_._2).reduce(_ ++ _)
    }

  def parallel: Parser[(`|`, Names)] =
    rep1sep(sequential, "|") ^^ { ss =>
      `|`(ss.map(_._1)*) -> ss.map(_._2).reduce(_ ++ _)
    }

  def sequential: Parser[(`.`, Names)] =
    prefixes ~ opt( leaf | "("~>choice<~")" ) ^^ {
      case pre ~ Some((end, free)) =>
        `.`(end, pre._1*) -> (pre._2._2 ++ (free &~ pre._2._1))
      case pre ~ _ =>
        `.`(∅, pre._1*) -> pre._2._2 // inaction
    }

  def leaf: Parser[(`-`, Names)] = agent() |
    "["~test~"]"~choice ^^ { // (mis)match
      case _ ~ cond ~ _ ~ t =>
        `?:`(cond._1, t._1, ∅) -> (cond._2 ++ t._2)
    } |
    "if"~test~"then"~choice~"else"~choice ^^ { // if then else
      case _ ~ cond ~ _ ~ t ~ _ ~ f =>
        `?:`(cond._1, t._1, f._1) -> (cond._2 ++ (t._2 ++ f._2))
    } |
    test~"?"~choice~":"~choice ^^ { // Elvis operator
      case cond ~ _ ~ t ~ _ ~ f =>
        `?:`(cond._1, t._1, f._1) -> (cond._2 ++ (t._2 ++ f._2))
    } |
    "!"~> opt( "."~> `μ.` <~"." ) ~ choice ^^ { // [guarded] replication
      case Some(μ) ~ (sum, free) =>
        `!`(Some(μ._1), sum) -> ((free &~ μ._2._1) ++ μ._2._2)
      case _ ~ (sum, free) =>
        `!`(None, sum) -> free
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

  def prefix: Parser[(Pre, (Names, Names))] = `μ.`<~"." |
    "ν"~>"("~>rep1sep(name, ",")<~")" ^^ { // restriction
      case ns if !ns.forall(_._1.isSymbol) =>
        throw PrefixChannelsParsingException(ns.filterNot(_._1.isSymbol).map(_._1)*)
      case ns =>
        ν(ns.map(_._1.asSymbol.name)*) -> (ns.map(_._2).reduce(_ ++ _), Names())
    }

  def test: Parser[(((λ, λ), Boolean), Names)] = "("~>test<~")" |
    name~("="|"≠")~name ^^ {
      case (lhs, free_lhs) ~ mismatch ~ (rhs, free_rhs) =>
        (lhs -> rhs -> (mismatch != "=")) -> (free_lhs ++ free_rhs)
    }

  def agent(binding: Boolean = false): Parser[(`(*)`, Names)] =
    qual ~ IDENT ~ opt( "("~>repsep(name, ",")<~")" ) ^^ {
      case qual ~ id ~ _ if binding && qual.nonEmpty =>
        throw EquationQualifiedException(id, qual)
      case _ ~ id ~ Some(params) if binding && !params.forall(_._1.isSymbol) =>
        throw EquationParamsException(id, params.filterNot(_._1.isSymbol).map(_._1.value)*)
      case qual ~ id ~ Some(params) =>
        `(*)`(λ(Symbol(id)), qual, params.map(_._1)*) -> params.map(_._2).foldLeft(Set.empty)(_ ++ _)
      case qual ~ id ~ _ =>
        `(*)`(λ(Symbol(id)), qual) -> Names()
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

  type Bind = (`(*)`, `+`)

  sealed trait AST extends Any

  case class `+`(choices: `|`*) extends AST

  object ∅ extends `+`():
    override def canEqual(that: Any): Boolean =
      that.isInstanceOf[`+`]

    override def equals(any: Any): Boolean = any match
      case that: `+` => that.choices.isEmpty
      case _ => false

  case class `|`(components: `.`*) extends AnyVal with AST

  case class `.`(end: `&`, prefixes: Pre*) extends AST

  sealed trait Pre extends Any

  case class ν(names: String*) extends AnyVal with Pre // forcibly

  case class τ(code: Option[Either[List[Enumerator], Term]]) extends AnyVal with Pre

  case class π(channel: λ,
               polarity: Boolean,
               code: Option[Either[List[Enumerator], Term]],
               names: λ*) extends Pre

  case class `?:`(cond: ((λ, λ), Boolean), t: `+`, f: `+`) extends AST

  case class `(*)`(identifier: λ,
                   qual: List[String],
                   params: λ*) extends AST

  case class `!`(guard: Option[μ], sum: `+`) extends AST

  case class λ(value: Any):
    val isSymbol: Boolean = value.isInstanceOf[Symbol]
    def asSymbol: Symbol = value.asInstanceOf[Symbol]

    val kind: String = value match {
      case _: Symbol => "channel name"
      case _: BigDecimal => "decimal number"
      case _: Boolean => "True False"
      case _: String => "string literal"
      case _: Expr => "Scalameta Term"
    }

  case class Expr(term: Term)


  // exceptions

  import Expression.ParsingException

  sealed class EquationParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class MainParsingException(params: Any*)
      extends EquationParsingException(s"Main has \"formal\" parameters (${params.mkString(", ")}), but it is spliced the command line arguments")

  case class EquationQualifiedException(id: String, qual: List[String])
      extends EquationParsingException(s"A qualified package ${qual.mkString(".")} is present in the left hand side of $id")

  case class EquationParamsException(id: String, params: Any*)
      extends EquationParsingException(s"The \"formal\" parameters (${params.mkString(", ")}) are not names in the left hand side of $id")

  case class EquationFreeNamesException(id: String, free: Names)
      extends EquationParsingException(s"The free names (${free.map(_.name).mkString(", ")}) in the right hand side are not formal parameters of the left hand side of $id")


  // functions

  def flatten[T <: AST](ast: T): T =

    inline given Conversion[AST, T] = _.asInstanceOf[T]

    ast match

      case `∅` => ∅

      case `+`(`|`(`.`(sum: `+`)), it*) =>
        val lhs = flatten(sum)
        val rhs = flatten(`+`(it*))
        `+`((lhs.choices ++ rhs.choices).filterNot(∅ == `+`(_))*)

      case `+`(par, it*) =>
        val lhs = `+`(flatten(par))
        val rhs = flatten(`+`(it*))
        `+`((lhs.choices ++ rhs.choices).filterNot(∅ == `+`(_))*)

      case `|`(`.`(`+`(par)), it*) =>
        val lhs = flatten(par)
        val rhs = flatten(`|`(it*))
        `|`((lhs.components ++ rhs.components)*)

      case `|`(seq, it*) =>
        val lhs = `|`(flatten(seq))
        val rhs = flatten(`|`(it*))
        `|`((lhs.components ++ rhs.components)*)

      case `.`(`+`(`|`(`.`(end, ps*))), it*) =>
        flatten(`.`(end, (it ++ ps)*))

      case `.`(end, it*) =>
        `.`(flatten(end), it*)

      case `?:`(cond, t, f) =>
        `?:`(cond, flatten(t), flatten(f))

      case `!`(None, sum) =>
        flatten(sum) match
          case `+`(`|`(`.`(end: `!`))) => end
          case it => `!`(None, it)

      case `!`(μ, sum) =>
        `!`(μ, flatten(sum))

      case _ => ast
