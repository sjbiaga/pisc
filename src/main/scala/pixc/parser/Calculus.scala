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

import scala.collection.mutable.{ ListBuffer => MutableList, LinkedHashSet => Set }

import scala.meta.{ Enumerator, Term }

import scala.util.parsing.combinator._

import StochasticPi.{ Act, Actions, nil, Names, Sum }
import Calculus._


class Calculus extends StochasticPi:

  def line: Parser[Either[Bind, Define]] =
    equation ^^ { Left(_) } | definition ^^ { Right(_) }

  def equation: Parser[Bind] =
    agent(true)~"="~choice ^^ {
      case (bind, binding) ~ _ ~ (sum, free)
        if (free &~ binding).nonEmpty =>
        throw EquationFreeNamesException(bind.identifier, free &~ binding)
      case (bind, _) ~ _ ~ (sum, _) =>
        bind -> sum.flatten
    }

  def definition: Parser[Define] =
    encoding ~ opt( "("~>rep1sep(name, ",")<~")" ) ~ opt( "{"~>rep1sep(name, ",")<~"}" ) ~"="~ choice ^^ {
      case (term, binding1) ~ binding2 ~ _bound ~ _ ~ (_sum, _free) =>
        val sum = _sum.flatten
        val free = (_free ++ sum.capitals)
          .filterNot { case Symbol(it) =>
            it.charAt(0).isUpper &&
            eqtn.exists { case (`(*)`(`it`, _), _) => true case _ => false }
          }
        val binding = binding2
          .map(binding1 ++ _.map(_._2).reduce(_ ++ _))
          .getOrElse(binding1)
        val bound = _bound.map(_.map(_._2).reduce(_ ++ _)).getOrElse(Names())
        if (free &~ (binding ++ bound)).nonEmpty
        then
          throw DefinitionFreeNamesException(code, free &~ (binding ++ bound))
        val const = binding2.map(_.map(_._2).reduce(_ ++ _)).getOrElse(Names())
        if !binding1.exists(_.name.charAt(0).isUpper)
        then
          eqtn :+= `(*)`("Self_" + code, (binding1 ++ const ++ bound).map(λ(_)).toSeq*) -> sum
        Encoding(code, term, const, bound) -> sum
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

  def leaf: Parser[(`-`, Names)] =
    "["~test~"]"~choice ^^ { // (mis)match
      case _ ~ cond ~ _ ~ t =>
        `?:`(cond._1, t._1, None) -> (cond._2 ++ t._2)
    } |
    "if"~test~"then"~choice~"else"~choice ^^ { // if then else
      case _ ~ cond ~ _ ~ t ~ _ ~ f =>
        `?:`(cond._1, t._1, Some(f._1)) -> (cond._2 ++ (t._2 ++ f._2))
    } |
    test~"?"~choice~":"~choice ^^ { // Elvis operator
      case cond ~ _ ~ t ~ _ ~ f =>
        `?:`(cond._1, t._1, Some(f._1)) -> (cond._2 ++ (t._2 ++ f._2))
    } |
    "!"~> opt( "."~>`μ.`<~"." ) ~ choice ^^ { // [guarded] replication
      case Some((π(λ(Symbol(ch)), λ(Symbol(par)), true, _, _), _)) ~ _ if ch == par =>
        throw GuardParsingException(ch)
      case Some(μ) ~ (sum, free) =>
        `!`(Some(μ._1), sum) -> ((free &~ μ._2._1) ++ μ._2._2)
      case _ ~ (sum, free) =>
        `!`(None, sum) -> free
    } |
    IDENT ~ ("{"~>rep1sep(name, ",")<~"}") ^^ { // pointed values
      case id ~ pointers =>
        `{}`(id, pointers.map(_._2).reduce(_ ++ _)) -> Names()
    } |
    agent() | // invocation
    expansion |
    ident("transaction") ~ ("["~> choice <~"]") ^^ { // transaction
      case name ~ (sum, free) =>
        `[]`(name, sum) -> (free - Symbol(name))
    }

  def expansion: Parser[(`[|]`, Names)] =
    regexMatch("""\[(\d*)\|""".r) >> { m =>
      val grp1 = m.group(1)
      val code = if grp1.isEmpty then 0 else grp1.toInt
      (expand(defn(code), s"|$grp1]") <~ s"|$grp1]") ~ opt( ("{"~>rep1sep(name, ",")<~"}") )
    } ^^ {
      case (it @ `[|]`(Encoding(_, _, const, bound), _, _), free) ~ Some(_pointers) =>
        val pointers = _pointers.map(_._2).reduce(_ ++ _)
        val assign = bound.map(_.name) zip pointers.map(_.name)
        given MutableList[(String, λ)]()
        it.copy(assign = Some(assign)).rename -> (free ++ const)
      case (it @ `[|]`(Encoding(_, _, const, _), _, _), free) ~ _ =>
        given MutableList[(String, λ)]()
        it.rename -> (free ++ const)
    }

  def expand(it: Define, end: String): Parser[(`[|]`, Names)] = ???

  def prefixes: Parser[(List[Pre], (Names, Names))] =
    rep(prefix) ^^ { ps =>
      val binding = ps.map(_._2._1)
      val free = ps.map(_._2._2)
        .zipWithIndex
        .foldLeft(Names()) { case (r, (ns, i)) =>
          ns.foldLeft(r) {
            case (r, n)
              if {
                val j = binding.indexWhere(_.contains(n))
                j < 0 || i <= j
              } => r + n
            case (r, _) => r
          }
        }
      ps.map(_._1) -> (if binding.nonEmpty then binding.reduce(_ ++ _) else Names(), free)
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
    IDENT ~ opt( "("~>rep1sep(name, ",")<~")" ) ^^ {
      case id ~ Some(params) if binding && !params.forall(_._1.isSymbol) =>
        throw EquationParamsException(id, params.filterNot(_._1.isSymbol).map(_._1.value)*)
      case "Self" ~ Some(params) =>
        self += code
        `(*)`("Self_" + code, params.map(_._1)*) -> params.map(_._2).reduce(_ ++ _)
      case "Self" ~ _ =>
        self += code
        `(*)`("Self_" + code) -> Names()
      case id ~ Some(params) =>
        id match
          case s"Self_$n" if (try { n.toInt; true } catch _ => false) =>
            self += n.toInt
          case _ =>
        `(*)`(id, params.map(_._1)*) -> params.map(_._2).reduce(_ ++ _)
      case id ~ _ =>
        id match
          case s"Self_$n" if (try { n.toInt; true } catch _ => false) =>
            self += n.toInt
          case _ =>
        `(*)`(id) -> Names()
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

  type Define = (Encoding, `+`)

  case class Encoding(code: Int, term: Term, const: Names, bound: Names):
    override def toString: String =
      if code == 0 then s"[| $term |]" else s"[$code| $term |$code]"

  sealed trait AST extends Any

  case class `+`(override val enabled: Actions,
                 choices: `|`*) extends AST with Sum:
    override def toString: String = choices.mkString(" + ")

  object ∅ extends `+`(nil):
    override def canEqual(that: Any): Boolean =
      that.isInstanceOf[`+`]

    override def equals(any: Any): Boolean = any match
      case that: `+` => that.choices.isEmpty
      case _ => false

    override def toString: String = "()"

  case class `|`(components: `.`*) extends AnyVal with AST:
    override def toString: String = components.mkString(" | ")

  case class `.`(end: `&`, prefixes: Pre*) extends AST:
    override def toString: String =
      prefixes.mkString(" ") + (if prefixes.isEmpty then "" else " ") + (if ∅ != end && end.isInstanceOf[`+`]
                                                                         then "(" + end + ")" else end)

  sealed trait Pre extends Any

  case class ν(names: String*) extends AnyVal with Pre: // forcibly
    override def toString: String = names.mkString("ν(", ", ", ")")

  case class τ(code: Option[Either[List[Enumerator], Term]],
               override val rate: Any)
      extends Pre with Act:
    override def toString: String = "τ."

  case class π(channel: λ,
               name: λ,
               polarity: Boolean,
               override val rate: Any,
               code: Option[Either[List[Enumerator], Term]])
      extends Pre with Act:
    override def toString: String =
      if polarity
      then "" + channel + "(" + name + ")."
      else "" + channel + "<" + name + ">."

  case class χ(name: String,
               sum: Option[`+`],
               override val rate: Any)
      extends Pre with Act:
    override def toString: String =
      if sum.isEmpty
      then "end(" + name + ")"
      else "start(" + name + ") [" + sum.get + "]"

  case class `?:`(cond: ((λ, λ), Boolean), t: `+`, f: Option[`+`]) extends AST:
    override def toString: String =
      val test = "" + cond._1._1 + (if cond._2 then " ≠ " else " = ") + cond._1._2
      if f.isEmpty
      then
        "[ " + test + " ]" + t
      else
        "if " + test + " " + t + " else " + f.get

  case class `!`(guard: Option[μ], sum: `+`) extends AST:
    override def toString: String = "!" + guard.map("." + _).getOrElse("") + sum

  case class `[|]`(encoding: Encoding,
                   sum: `+`,
                   assign: Option[Set[(String, String)]] = None) extends AST:
    override def toString: String =
      s"""$encoding${assign.map{_.map(_ + "->" + _).mkString("{", ", ", "}")}.getOrElse("")} = $sum"""

  case class `{}`(identifier: String,
                  pointers: Names) extends AST:
    override def toString: String = s"""$identifier{${pointers.map(_.name).mkString(", ")}}"""

  case class `(*)`(identifier: String,
                   params: λ*) extends AST:
    override def toString: String = s"$identifier(${params.mkString(", ")})"

  case class `[]`(name: String, sum: `+`) extends AST:
    override def toString: String = name + (if ∅ == sum then " [ ]" else " [ " + sum + " ]")

  case class λ(value: Any):
    val isSymbol: Boolean = value.isInstanceOf[Symbol]
    def asSymbol: Symbol = value.asInstanceOf[Symbol]

    val kind: String = value match
      case _: Symbol => "channel name"
      case _: BigDecimal => "decimal number"
      case _: Boolean => "True False"
      case _: String => "string literal"
      case _: Expr => "Scalameta Term"

    override def toString: String = value match
      case it: Symbol => it.name
      case it: BigDecimal => "" + it
      case it: Boolean => it.toString.capitalize
      case it: String => it
      case it: Expr => "" + it

  case class Expr(term: Term):
    override def toString: String = "/*" + term + "*/"


  // exceptions

  import Expression.ParsingException
  import StochasticPi.PrefixParsingException

  class EquationParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class StartParsingException(id: String, by: String)
      extends EquationParsingException(s"$id leads to a start transaction prefix by $by")

  case class EquationParamsException(id: String, params: Any*)
      extends EquationParsingException(s"The \"formal\" parameters (${params.mkString(", ")}) are not names in the left hand side of $id")

  case class EquationFreeNamesException(id: String, free: Names)
      extends EquationParsingException(s"The free names (${free.map(_.name).mkString(", ")}) in the right hand side are not formal parameters of the left hand side of $id")

  case class DefinitionFreeNamesException(code: Int, free: Names)
      extends EquationParsingException(s"The free names (${free.map(_.name).mkString(", ")}) in the right hand side are not formal parameters of the left hand side of encoding $code")

  case class PrefixChannelsParsingException(names: λ*)
      extends PrefixParsingException(s"${names.map(_.value).mkString(", ")} are not channel names but ${names.map(_.kind).mkString(", ")}")

  case class GuardParsingException(name: String)
      extends PrefixParsingException(s"$name is both the channel name and the binding parameter name in an input guard")


  // functions

  extension[T <: AST](ast: T)

    def flatten: T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case `∅` => ∅

        case `+`(_, `|`(`.`(sum: `+`)), it*) =>
          val lhs = sum.flatten
          val rhs = `+`(nil, it*).flatten
          `+`(nil, (lhs.choices ++ rhs.choices).filterNot(∅ == `+`(nil, _))*)

        case `+`(_, par, it*) =>
          val lhs = `+`(nil, par.flatten)
          val rhs = `+`(nil, it*).flatten
          `+`(nil, (lhs.choices ++ rhs.choices).filterNot(∅ == `+`(nil, _))*)

        case `|`(`.`(`+`(_, par)), it*) =>
          val lhs = par.flatten
          val rhs = `|`(it*).flatten
          `|`((lhs.components ++ rhs.components)*)

        case `|`(seq, it*) =>
          val lhs = `|`(seq.flatten)
          val rhs = `|`(it*).flatten
          `|`((lhs.components ++ rhs.components)*)

        case `.`(`+`(_, `|`(`.`(end, ps*))), it*) =>
          `.`(end, (it ++ ps)*).flatten

        case `.`(end, _it*) =>
          val it = _it
            .map {
              case xa @ χ(_, Some(sum), _) =>
                xa.copy(sum = Some(sum.flatten))
              case it => it
            }
          `.`(end.flatten, it*)

        case `?:`(cond, t, f) =>
          `?:`(cond, t.flatten, f.map(_.flatten))

        case `!`(None, sum) =>
          sum.flatten match
            case `+`(_, `|`(`.`(end: `!`))) => end
            case it => `!`(None, it)

        case `!`(μ, sum) =>
          `!`(μ, sum.flatten)

        case `[|]`(encoding, sum, assign) =>
          `[|]`(encoding, sum.flatten, assign)

        case `[]`(name, sum) =>
          sum.flatten match
            case it @ `+`(_, _) =>
              `[]`(name, it)
            case it =>
              `[]`(name, `+`(nil, `|`(`.`(it, τ(None, Some(-Long.MaxValue))))))

        case _ => ast

    def capitals: Names =

      ast match

        case `∅` => Set.empty

        case `+`(_, it*) => it.map(_.capitals).reduce(_ ++ _)

        case `|`(it*) => it.map(_.capitals).reduce(_ ++ _)

        case `.`(end, _*) =>
          end.capitals

        case `?:`(_, t, f) =>
          t.capitals ++ f.map(_.capitals).getOrElse(Names())

        case `!`(_, sum) =>
          sum.capitals

        case `[|]`(_, sum, _) =>
          sum.capitals

        case `{}`(id, _) => Set(Symbol(id))

        case `(*)`(id) => Set(Symbol(id))

        case `[]`(_, sum) =>
          sum.capitals

        case _ => Set.empty

    def rename(using r: MutableList[(String, λ)]): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case `∅` => ∅

        case `+`(_, it*) =>
          `+`(nil, it.map(_.rename)*)

        case `|`(it*) =>
          `|`(it.map(_.rename)*)

        case `.`(end, _it*) =>
          val it = _it.map {
            case it @ π(λ(Symbol(ch)), λ(Symbol(par)), true, _, _) =>
              val υidυ = par.replaceAll("_υ.*υ", "") + id
              r.prepend(par -> λ(Symbol(υidυ)))
              val ch2 = r.find(_._1 == ch).map(_._2).getOrElse(λ(Symbol(ch)))
              val par2 = r.find(_._1 == par).get._2
              it.copy(channel = ch2, name = par2)
            case it @ π(λ(Symbol(ch)), λ(Symbol(arg)), false, _, _) =>
              val ch2 = r.find(_._1 == ch).map(_._2).getOrElse(λ(Symbol(ch)))
              val arg2 = r.find(_._1 == arg).map(_._2).getOrElse(λ(Symbol(arg)))
              it.copy(channel = ch2, name = arg2)
            case ν(names*) =>
              names
                .reverse
                .foreach { it =>
                  val υidυ = it.replaceAll("_υ.*υ", "") + id
                  r.prepend(it -> λ(Symbol(υidυ)))
                }
              ν(names.map { it => r.find(_._1 == it).get._2.asSymbol.name }*)
            case it => it
          }
          val seq = `.`(end.rename, it*)
          it.reverse.foreach {
            case π(_, _, true, _, _) =>
              r.remove(0)
            case ν(names*) =>
              r.remove(0, names.size)
            case _ =>
          }
          seq

        case `?:`(((λ(Symbol(lhs)), λ(Symbol(rhs))), m), t, f) =>
          val lhs2 = r.find(_._1 == lhs).map(_._2).getOrElse(λ(Symbol(lhs)))
          val rhs2 = r.find(_._1 == rhs).map(_._2).getOrElse(λ(Symbol(rhs)))
          `?:`(((lhs2, rhs2), m), t.rename, f.map(_.rename))

        case `?:`(((λ(Symbol(lhs)), rhs), m), t, f) =>
          val lhs2 = r.find(_._1 == lhs).map(_._2).getOrElse(λ(Symbol(lhs)))
          `?:`(((lhs2, rhs), m), t.rename, f.map(_.rename))

        case `?:`(((lhs, λ(Symbol(rhs))), m), t, f) =>
          val rhs2 = r.find(_._1 == rhs).map(_._2).getOrElse(λ(Symbol(rhs)))
          `?:`(((lhs, rhs2), m), t.rename, f.map(_.rename))

        case `?:`(cond, t, f) =>
          `?:`(cond, t.rename, f.map(_.rename))

        case `!`(Some(it @ π(λ(Symbol(ch)), λ(Symbol(par)), true, _, _)), sum) =>
          val υidυ = par.replaceAll("_υ.*υ", "") + id
          r.prepend(par -> λ(Symbol(υidυ)))
          val ch2 = r.find(_._1 == ch).map(_._2).getOrElse(λ(Symbol(ch)))
          val par2 = r.find(_._1 == par).get._2
          val rep = `!`(Some(it.copy(channel = ch2, name = par2)), sum.rename)
          r.remove(0)
          rep

        case `!`(Some(it @ π(λ(Symbol(ch)), λ(Symbol(arg)), false, _, _)), sum) =>
          val ch2 = r.find(_._1 == ch).map(_._2).getOrElse(λ(Symbol(ch)))
          val arg2 = r.find(_._1 == arg).map(_._2).getOrElse(λ(Symbol(arg)))
          `!`(Some(it.copy(channel = ch2, name = arg2)), sum.rename)

        case `!`(guard, sum) =>
          `!`(guard, sum.rename)

        case `[|]`(encoding, sum, Some(assign)) =>
          val assign2 = assign
            .map { case (bound, pointer) =>
              val pointer2 = r.find(_._1 == pointer).map(_._2.asSymbol.name).getOrElse(pointer)
              val υidυ = bound.replaceAll("_υ.*υ", "") + id
              r.prepend(bound -> λ(Symbol(υidυ)))
              val bound2 = r.find(_._1 == bound).get._2.asSymbol.name
              bound2 -> pointer2
            }
          val encoding2 = encoding.copy(bound = assign2.map(_._1).map(Symbol(_)))
          val enc = `[|]`(encoding2, sum.rename, Some(assign2))
          r.remove(0, assign.size)
          enc

        case `[|]`(encoding, sum, _) =>
          `[|]`(encoding, sum.rename, None)

        case `[]`(name, sum) =>
          `[]`(name, sum.rename)

        case _: `{}` => ???

        case `(*)`(id, params*) =>
          val args = params
            .map {
              case λ(Symbol(it)) => r.find(_._1 == it).map(_._2).getOrElse(λ(Symbol(it)))
              case it => it
            }

          `(*)`(id, args*)

  private var _id = scala.collection.mutable.Seq('0')
  private var _ix = 0
  /**
    * @return unique identifiers of the form "_υ[0-9a-zA-Z]+υ"
    */
  def id: String =
    var reset = false
    while _ix >= 0 && _id(_ix) == 'Z'
    do
      _id(_ix) = '0'
      _ix -= 1
      reset = true
    if _ix < 0
    then
      _id :+= '1'
    else
      _id(_ix) match
        case 'z' =>
          _id(_ix) = 'A'
        case '9' =>
          _id(_ix) = 'a'
        case it =>
          _id(_ix) = (it + 1).toChar
    if reset then _ix = _id.size - 1
    "_υ" + _id.mkString + "υ"
