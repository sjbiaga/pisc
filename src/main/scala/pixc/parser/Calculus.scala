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

package pixc
package parser

import scala.util.parsing.combinator._

import StochasticPi.{ Act, Actions, nil, Names, State, PrefixParsingException }
import Calculus._

import scala.meta.{ Enumerator, Term }


class Calculus extends StochasticPi:

  def equation: Parser[Bind] =
    agent(true)~"="~choice ^^ {
      case (bind, bound) ~ _ ~ (sum, free)
        if (free &~ bound).nonEmpty =>
        throw EquationFreeNamesException(bind.identifier.asSymbol.name, free &~ bound)
      case (bind, _) ~ _ ~ (sum, _) =>
        bind -> flatten(sum)
    }

  def choice: Parser[(`+`, Names)] =
    rep1sep(parallel, "+") ^^ { ps =>
      `+`(nil, ps.map(_._1)*) -> ps.map(_._2).reduce(_ ++ _)
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

  def leaf: Parser[(`-`, Names)] = agent() | // invocation
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
    "!"~> opt( "."~>`μ.`<~"." ) ~ choice ^^ { // [guarded] replication
      case Some(μ) ~ (sum, free) =>
        `!`(Some(μ._1), sum) -> ((free &~ μ._2._1) ++ μ._2._2)
      case _ ~ (sum, free) =>
        `!`(None, sum) -> free
    } |
    ident("transaction") ~ ("["~> choice <~"]") ^^ { // transaction
      case name ~ (sum, free) =>
        `[]`(name, sum) -> (free - Symbol(name))
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
    "ν"~>"("~>rep1sep(name, ",")<~")" ^^ { // restriction
      case ns if !ns.forall(_._1.isSymbol) =>
        throw PrefixChannelsParsingException(ns.filterNot(_._1.isSymbol).map(_._1)*)
      case ns =>
        ν(ns.map(_._1.asSymbol.name)*) -> (ns.map(_._2).reduce(_ ++ _), Names())
    } |
    "start"~> ("("~> ident("transaction") <~")") ~ opt("@"~>rate) ~ ("["~> choice <~"]") <~"." ^^ {
      case name ~ r ~ (sum, free) =>
        χ(name, Some(sum), r.getOrElse(1L)) -> (Names() + Symbol(name), free - Symbol(name))
    } |
    "end"~> ("("~> ident("transaction") <~")") ~ opt("@"~>rate) <~"." ^^ {
      case name ~ r =>
        χ(name, None, r.getOrElse(1L)) -> (Names(), Names() + Symbol(name))
    } |
    `μ.`<~"."

  def test: Parser[(((λ, λ), Boolean), Names)] = "("~>test<~")" |
    name~("="|"≠")~name ^^ {
      case (lhs, free_lhs) ~ mismatch ~ (rhs, free_rhs) =>
        (lhs -> rhs -> (mismatch != "=")) -> (free_lhs ++ free_rhs)
    }

  def agent(binding: Boolean = false): Parser[(`(*)`, Names)] =
    IDENT ~ opt( "("~>repsep(name, ",")<~")" ) ^^ {
      case id ~ Some(params) if binding && !params.forall(_._1.isSymbol) =>
        throw EquationParamsException(id, params.filterNot(_._1.isSymbol).map(_._1.value)*)
      case id ~ Some(params) =>
        `(*)`(λ(Symbol(id)), params.map(_._1)*) -> params.map(_._2).foldLeft(Names())(_ ++ _)
      case id ~ _ =>
        `(*)`(λ(Symbol(id))) -> Names()
    }

  /**
   * Agent identifiers start with upper case.
   * @return
   */
  def IDENT: Parser[String] =
      "" ~> // handle whitespace
      rep1(acceptIf(Character.isUpperCase)("agent identifier expected but '" + _ + "' found"),
          elem("agent identifier part", { (ch: Char) => Character.isJavaIdentifierPart(ch) || ch == '\'' || ch == '"' })) ^^ (_.mkString)


object Calculus:

  type Bind = (`(*)`, `+`)

  sealed trait AST extends Any

  case class `+`(override val enabled: Actions,
                 choices: `|`*) extends AST with State

  object ∅ extends `+`(nil):
    override def canEqual(that: Any): Boolean =
      that.isInstanceOf[`+`]

    override def equals(any: Any): Boolean = any match
      case that: `+` => that.choices.isEmpty
      case _ => false

  case class `|`(components: `.`*) extends AnyVal with AST

  case class `.`(end: `&`, prefixes: Pre*) extends AST

  sealed trait Pre extends Any

  case class ν(names: String*) extends AnyVal with Pre // forcibly

  case class τ(code: Option[Either[List[Enumerator], Term]],
               override val rate: Any)
      extends Pre with Act with State:
    override val enabled: Actions = Actions(this)

  case class π(channel: λ,
               name: λ,
               polarity: Boolean,
               override val rate: Any,
               code: Option[Either[List[Enumerator], Term]])
      extends Pre with Act with State:
    override val enabled: Actions = Actions(this)

  case class χ(name: String,
               sum: Option[`+`],
               override val rate: Any)
      extends Pre with Act with State:
    override val enabled: Actions = Actions(this)

  case class `?:`(cond: ((λ, λ), Boolean), t: `+`, f: `+`) extends AST

  case class `(*)`(identifier: λ,
                   params: λ*) extends AST:
    override def canEqual(that: Any): Boolean =
      that.isInstanceOf[`(*)`]

    override def equals(any: Any): Boolean = any match
      case that: `(*)` => this.identifier.asSymbol.name == that.identifier.asSymbol.name
                       && this.params.map(_.asSymbol.name) == that.params.map(_.asSymbol.name)
      case _ => false

    override def hashCode(): Int = (identifier.asSymbol.name, params.map(_.asSymbol.name)).##

  case class `!`(guard: Option[μ], sum: `+`) extends AST

  case class `[]`(name: String, sum: `+`) extends AST

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

  class EquationParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class StartParsingException(id: String, by: String)
      extends EquationParsingException(s"$id leads to a start transaction prefix by $by")

  case class EquationParamsException(id: String, params: Any*)
      extends EquationParsingException(s"The \"formal\" parameters (${params.mkString(", ")}) are not names in the left hand side of $id")

  case class EquationFreeNamesException(id: String, free: Names)
      extends EquationParsingException(s"The free names (${free.map(_.name).mkString(", ")}) in the right hand side are not formal parameters of the left hand side of $id")

  case class PrefixChannelsParsingException(names: λ*)
      extends PrefixParsingException(s"${names.map(_.value).mkString(", ")} are not channel names but ${names.map(_.kind).mkString(", ")}")


  // functions

  def flatten[T <: AST](ast: T): T =

    inline given Conversion[AST, T] = _.asInstanceOf[T]

    ast match

      case `∅` => ∅

      case `+`(_, `|`(`.`(sum: `+`)), it*) =>
        val lhs = flatten(sum)
        val rhs = flatten(`+`(nil, it*))
        `+`(nil, (lhs.choices ++ rhs.choices).filterNot(∅ == `+`(nil, _))*)

      case `+`(_, par, it*) =>
        val lhs = `+`(nil, flatten(par))
        val rhs = flatten(`+`(nil, it*))
        `+`(nil, (lhs.choices ++ rhs.choices).filterNot(∅ == `+`(nil, _))*)

      case `|`(`.`(`+`(_, par)), it*) =>
        val lhs = flatten(par)
        val rhs = flatten(`|`(it*))
        `|`((lhs.components ++ rhs.components)*)

      case `|`(seq, it*) =>
        val lhs = `|`(flatten(seq))
        val rhs = flatten(`|`(it*))
        `|`((lhs.components ++ rhs.components)*)

      case `.`(`+`(_, `|`(`.`(end, ps*))), it*) =>
        flatten(`.`(end, (it ++ ps)*))

      case `.`(end, it*) =>
        val ps = it
          .map {
            case xa @ χ(_, Some(sum), _) =>
              xa.copy(sum = Some(flatten(sum)))
            case p => p
          }
        `.`(flatten(end), ps*)

      case `?:`(cond, t, f) =>
        `?:`(cond, flatten(t), flatten(f))

      case `!`(None, sum) =>
        flatten(sum) match
          case `+`(_, `|`(`.`(end: `!`))) => end
          case it => `!`(None, it)

      case `!`(μ, sum) =>
        `!`(μ, flatten(sum))

      case `[]`(name, sum) =>
        flatten(sum) match
          case it @ `+`(_, _) =>
            `[]`(name, it)
          case it =>
            `[]`(name, `+`(nil, `|`(`.`(it, τ(None, Some(-Long.MaxValue))))))

      case _ => ast