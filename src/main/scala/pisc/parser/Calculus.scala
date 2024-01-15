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

import scala.util.parsing.combinator._

import Pi.{ Names, PrefixChannelParsingException }
import Calculus._

import scala.meta.Term


class Calculus extends Pi:

  def equation: Parser[Bind] =
    agent(true)~"="~choice ^^ {
      case (bind, bound) ~ _ ~ (sum, free)
        if (free &~ bound).nonEmpty =>
        throw EquationFreeNamesException(bind.identifier.asSymbol.name, free &~ bound)
      case (bind, _) ~ _ ~ (sum, _) =>
        var f = flatten(sum)
        while f._2
        do
          f = flatten(f._1)
        bind -> f._1.asInstanceOf[`+`]
    }

  def choice: Parser[(`+`, Names)] = "("~>choice<~")" |
    rep1sep(parallel, "+") ^^ { ps =>
      `+`(ps.map(_._1): _*) -> ps.map(_._2).reduce(_ ++ _)
    }

  def parallel: Parser[(`|`, Names)] = "("~>parallel<~")" |
    rep1sep(sequential, "|") ^^ { ss =>
      `|`(ss.map(_._1): _*) -> ss.map(_._2).reduce(_ ++ _)
    }

  def sequential: Parser[(`.`, Names)] =
    prefixes ~ opt( leaf | "("~>choice<~")" ) ^^ {
      case (pre, _) ~ None if pre.isEmpty =>
        throw EmptyParsingException
      case pre ~ Some((end: `&`, free: Names)) =>
        `.`(end, pre._1: _*) -> (pre._2._2 ++ (free &~ pre._2._1))
      case pre ~ _ =>
        `.`(`𝟎`, pre._1: _*) -> pre._2._2
    }

  def leaf: Parser[(`-`, Names)] = agent() |
    "𝟎" ^^ { _ => (`𝟎`, Names()) } |
    "["~test~"]"~choice ^^ { // (mis)match
      case _ ~ cond ~ _ ~ t =>
        `?:`(cond._1, t._1, `𝟎`) -> (cond._2 ++ t._2)
    } |
    "if"~test~"then"~choice~"else"~choice ^^ { // if then else
      case _ ~ cond ~ _ ~ t ~ _ ~ f =>
        `?:`(cond._1, t._1, f._1) -> (cond._2 ++ (t._2 ++ f._2))
    } |
    test~"?"~choice~":"~choice ^^ { // Elvis operator
      case cond ~ _ ~ t ~ _ ~ f =>
        `?:`(cond._1, t._1, f._1) -> (cond._2 ++ (t._2 ++ f._2))
    } |
    "!"~>choice ^^ { // replication
      case (sum, free) => `!`(sum) -> free
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

  def prefix: Parser[(Pre, (Names, Names))] = `π.`<~"." |
    "ν"~>"("~>name<~")" ^^ { // restriction i.e. new name
      case (ch, _) if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case (ch, name) =>
        ν(ch) -> (name, Names())
    }

  def test: Parser[(((λ, λ), Boolean), Names)] = "("~>test<~")" |
    name~("="|"≠")~name ^^ {
      case (lhs, free_lhs) ~ mismatch ~ (rhs, free_rhs) =>
        (lhs -> rhs -> (mismatch != "=")) -> (free_lhs ++ free_rhs)
    }

  def agent(binding: Boolean = false): Parser[(`()`, Names)] =
    qual ~ IDENT ~ opt( "("~>repsep(name, ",")<~")" ) ^^ {
      case qual ~ id ~ _ if binding && qual.nonEmpty =>
        throw EquationQualifiedException(id, qual)
      case _ ~ id ~ Some(params) if binding && !params.forall(_._1.isSymbol) =>
        throw EquationParamsException(id, params.filterNot(_._1.isSymbol).map(_._1.value):_ *)
      case qual ~ id ~ Some(params) =>
        `()`(λ(Symbol(id)), qual, params.map(_._1): _*) -> params.map(_._2).foldLeft(Set.empty)(_ ++ _)
      case qual ~ id ~ _ =>
        `()`(λ(Symbol(id)), qual) -> Names()
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

  type Bind = (`()`, `+`)

  sealed trait AST extends Any

  case class `+`(choices: `|`*) extends AST

  object `𝟎` extends `+`(`|`())

  case class `|`(components: `.`*) extends AnyVal with AST

  case class `.`(process: `&`, prefixes: Pre*) extends AST

  sealed trait Pre extends Any with AST

  case class ν(name: λ) extends AnyVal with Pre // forcibly

  case class τ(term: Option[Term]) extends AnyVal with Pre

  case class π(channel: λ, name: λ, polarity: Boolean) extends Pre

  case class `?:`(cond: ((λ, λ), Boolean), t: `+`, f: `+`) extends AST

  case class `()`(identifier: λ,
                  qual: List[String],
                  params: λ*) extends AST

  case class `!`(sum: `+`) extends AnyVal with AST

  case class λ(value: Any) extends AST:
    val isSymbol: Boolean = value.isInstanceOf[Symbol]
    def asSymbol: Symbol = value.asInstanceOf[Symbol]

    val kind: String = value match {
      case _: Symbol => "channel name"
      case _: BigDecimal => "decimal number"
      case _: Boolean => "True False"
      case _: String => "string literal"
      case _: Expr => "scalameta term"
    }

  case class Expr(term: Term)


  // exceptions

  class ParsingException(msg: String, cause: Throwable = null)
      extends RuntimeException(msg, cause)

  sealed class EquationParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class EquationQualifiedException(id: String, qual: List[String])
      extends EquationParsingException(s"A qualified package ${qual.mkString(".")} is present in the left hand side of $id")

  case class EquationParamsException(id: String, params: Any*)
      extends EquationParsingException(s"The \"formal\" parameters (${params.mkString(", ")}) are not names in the left hand side of $id")

  case class EquationFreeNamesException(id: String, free: Names)
      extends EquationParsingException(s"The free names (${free.map(_.name).mkString(", ")}) in the right hand side are not formal parameters of the left hand side of $id")

  case object EmptyParsingException
      extends ParsingException("Instead of an empty expression there must be at least 𝟎 in place")


  // functions

  val flatten: AST => (AST, Boolean) = _ match

    case it: `𝟎`.type => it -> false

    case `+`(`|`(`.`(sum: `+`, ps*), ss*), it*)
        if ps.isEmpty && ss.isEmpty =>
      val (lhs, _) = flatten(sum).asInstanceOf[(`+`, Boolean)]
      if it.isEmpty
      then
        `+`(lhs.choices: _*) -> true
      else
        val (rhs, _) = flatten(`+`(it: _*)).asInstanceOf[(`+`, Boolean)]
        `+`((lhs.choices ++ rhs.choices): _*) -> true

    case `+`(lhs, it*)
        if it.nonEmpty =>
      val (rhs, flag) = flatten(`+`(it: _*)).asInstanceOf[(`+`, Boolean)]
      `+`((lhs +: rhs.choices): _*) -> flag

    case `+`(par, _*) =>
      val f = flatten(par).asInstanceOf[(`|`, Boolean)]
      `+`(f._1) -> f._2

    case it @ `|`(`.`(_: `𝟎`.type), _*) => it -> false

    case `|`(`.`(`+`(`|`(ss*), p*), ps*), it*)
        if p.isEmpty && ps.isEmpty =>
      if it.isEmpty
      then
        `|`(ss: _*) -> true
      else
        val (rhs, _) = flatten(`|`(it: _*)).asInstanceOf[(`|`, Boolean)]
        `|`((ss ++ rhs.components): _*) -> true

    case `|`(lhs, it*)
        if it.nonEmpty =>
      val (rhs, flag) = flatten(`|`(it: _*)).asInstanceOf[(`|`, Boolean)]
      `|`((lhs +: rhs.components): _*) -> flag

    case `|`(`.`(end, ps*), _*) =>
      val f = flatten(end).asInstanceOf[(`&`, Boolean)]
      `|`(`.`(f._1, ps: _*)) -> f._2

    case `?:`(cond, t, f) =>
      val t_f = flatten(t).asInstanceOf[(`+`, Boolean)]
      val f_f = flatten(f).asInstanceOf[(`+`, Boolean)]
      `?:`(cond, t_f._1, f_f._1) -> (t_f._2 || f_f._2)

    case `!`(sum) =>
      val f = flatten(sum).asInstanceOf[(`+`, Boolean)]
      `!`(f._1) -> f._2

    case it => it -> false
