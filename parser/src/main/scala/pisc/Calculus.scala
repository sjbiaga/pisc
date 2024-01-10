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

import scala.io.Source
import scala.util.parsing.combinator._

import Pi.{ Names, PrefixChannelParsingException }
import Calculus._


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
    prefixes ~ opt( "𝟎" | "("~>choice<~")" | agent() ) ^^ {
      case (pre, _) ~ None if pre.isEmpty =>
        throw EmptyParsingException
      case pre ~ Some((sum: `+`, free: Names)) =>
        `.`(sum, pre._1: _*) -> (pre._2._2 ++ (free &~ pre._2._1))
      case pre ~ Some((call: `()`, free: Names)) =>
        `.`(call, pre._1: _*) -> (pre._2._2 ++ (free &~ pre._2._1))
      case pre ~ _ =>
        `.`(`𝟎`, pre._1: _*) -> pre._2._2
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

  def prefix: Parser[(Pre, (Names, Names))] = `π.` |
    "ν"~>"("~>name<~")" ^^ { // restriction i.e. new name
      case ch if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case ch =>
        ν(ch) -> (Names(ch), Names())
    } |
    "["~test~"]"~choice ^^ { // (mis)match
      case _ ~ cond ~ _ ~ t =>
        `[]`(cond, t._1) -> (Names(), Names(cond._1._1, cond._1._2) ++ t._2)
    } |
    "if"~test~"then"~choice~"else"~choice ^^ { // if then else
      case _ ~ cond ~ _ ~ t ~ _ ~ f =>
        `?:`(cond, t._1, f._1) -> (Names(), Names(cond._1._1, cond._1._2) ++ (t._2 ++ f._2))
    } |
    test~"?"~choice~":"~choice ^^ { // Elvis operator
      case cond ~ _ ~ t ~ _ ~ f =>
        `?:`(cond, t._1, f._1) -> (Names(), Names(cond._1._1, cond._1._2) ++ (t._2 ++ f._2))
    } |
    "!"~>choice ^^ { // replication
      case (sum, free) => `!`(sum) -> (Names(), free)
    }

  def test: Parser[((λ, λ), Boolean)] = "("~>test<~")" |
    name~("="|"≠")~name ^^ {
      case lhs ~ mismatch ~ rhs => lhs -> rhs -> (mismatch != "=")
    }

  def agent(binding: Boolean = false): Parser[(`()`, Names)] =
    qual ~ IDENT ~ opt( "("~>repsep(name, ",")<~")" ) ^^ {
      case qual ~ id ~ _ if binding && qual.nonEmpty =>
        throw EquationQualifiedException(id, qual)
      case _ ~ id ~ Some(params) if binding && !params.forall(_.isSymbol) =>
        throw EquationParamsException(id, params.filterNot(_.isSymbol).map(_.value):_ *)
      case qual ~ id ~ Some(params) =>
        `()`(λ(Symbol(id)), qual, params: _*) -> Names(params: _*)
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


object Calculus extends Calculus:

  type Bind = (`()`, `+`)

  sealed trait AST extends Any

  case class `+`(choices: `|`*) extends AnyVal with AST

  val `𝟎` = `+`(`|`())

  case class `|`(components: `.`*) extends AnyVal with AST

  case class `.`(process: AST, prefixes: Pre*) extends AST

  sealed trait Pre extends Any with AST

  case class ν(name: λ) extends AnyVal with Pre // forcibly

  case object τ extends Pre

  case class π(channel: λ, name: λ, polarity: Boolean) extends Pre

  case class `[]`(cond: ((λ, λ), Boolean), t: `+`) extends Pre // forcibly

  case class `?:`(cond: ((λ, λ), Boolean), t: `+`, f: `+`) extends Pre // forcibly

  case class `!`(sum: `+`) extends Pre // forcibly

  case class `()`(identifier: λ, qual: List[String], params: λ*) extends AST

  case class λ(value: AnyRef) extends AST:
    val isSymbol: Boolean = value.isInstanceOf[Symbol]
    def asSymbol: Symbol = value.asInstanceOf[Symbol]

    val kind: String = value match {
      case _: Symbol => "channel name"
      case _: String => "scala value"
      case _: Expr => "scala expression"
    }

  case class Expr(expression: String)


  // exceptions

  class ParsingException(msg: String, cause: Throwable = null)
      extends RuntimeException(msg, cause)

  sealed class EquationParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class EquationQualifiedException(id: String, qual: List[String])
      extends EquationParsingException(s"A qualified package ${qual.mkString(".")} is present in the left hand side of $id")

  case class EquationParamsException(id: String, params: AnyRef*)
      extends EquationParsingException(s"The \"formal\" parameters (${params.mkString(", ")}) are not names in the left hand side of $id")

  case class EquationFreeNamesException(id: String, free: Names)
      extends EquationParsingException(s"The free names (${free.map(_.name).mkString(", ")}) in the right hand side are not formal parameters of the left hand side of $id")

  case object EmptyParsingException
      extends ParsingException("Instead of an empty expression there must be at least 𝟎 in place")


  // functions

  val flatten: AST => (AST, Boolean) = _ match

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

    case `|`(`.`(ast, ps*), _*) =>
      val f = flatten(ast)
      `|`(`.`(f._1, ps: _*)) -> f._2

    case `[]`(cond, t) =>
      val f = flatten(t).asInstanceOf[(`+`, Boolean)]
      `[]`(cond, f._1) -> f._2

    case `?:`(cond, t, f) =>
      val t_f = flatten(t).asInstanceOf[(`+`, Boolean)]
      val f_f = flatten(f).asInstanceOf[(`+`, Boolean)]
      `?:`(cond, t_f._1, f_f._1) -> (t_f._2 || f_f._2)

    case `!`(sum) =>
      val f = flatten(sum).asInstanceOf[(`+`, Boolean)]
      `!`(f._1) -> f._2

    case it => it -> false


  def apply(source: Source): List[Either[String, Bind]] = source
    .getLines()
    .foldLeft(List[String]("")) {
      case (r, l) if l.endsWith("\\") => r.init :+ (r.last + l.stripSuffix("\\"))
      case (r, l) => r :+ l
    }
    .filterNot(_.matches("^ *#.*")) // commented lines
    .filterNot(_.isBlank) // empty lines
    .map { it =>
      if it.matches("^ *@.*")
      then // Scala
        Left(it.replaceFirst("^([ ]*)@(.*)$", "$1$2"))
      else // Pi
        parseAll(equation, it) match
          case Success(result, _) => Right(result)
          case failure: NoSuccess => scala.sys.error(failure.msg)
    }
    .toList
