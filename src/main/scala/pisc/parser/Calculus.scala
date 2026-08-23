/*
 * Copyright (c) 2023-2026 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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

import scala.meta.{ Pat, Term, Type }

import emitter.shared.Meta.rateʹ

import Expression.Code
import StochasticPi.*
import Calculus.*
import Encoding.*
import scala.util.parsing.combinator.pisc.parser.Expansion.Duplications


abstract class Calculus extends StochasticPi:

  def equation(using Duplications): Parser[Bind] =
    invocation(true) >> {
      case (bind, _) if _settings.exclude =>
        ".*".r ^^ { _ => bind -> ∅() }
      case (bind, bound) =>
        _code = -1
        _directive = None
        given Bindings = Bindings() ++ bound.map(_ -> Occurrence(None, pos()))
        given Int = 1
        "="~> choice ^^ {
          case (_sum, _free) =>
            val sum = _sum.flatten
            val free = _free ++ sum.capitals
            if (free &~ bound).nonEmpty
            then
              throw EquationFreeNamesException(bind.identifier, free &~ bound)
            if _settings.traces.isDefined
            then
              bind -> sum.labelʹ(using bind.identifier)
            else
              bind -> sum
        }
    }

  def choice(using Bindings, Duplications, Int): Parser[(+, Names)] =
    scale >> { scaling =>
      val scalingʹ = scaling.abs
      given Int = if scalingʹ == 1 then summon[Int] else scalingʹ
      rep1sep(parallel, "+") ^^ { _.unzip match
        case (it, ns) =>
          if scalingʹ == 0
          then
            ∅() -> Names()
          else if _settings.scaling && emitter.canScale
          then
            `+`(scaling, it*) -> ns.reduce(_ ++ _)
          else
            `+`(-1, List.fill(scalingʹ)(it).reduce(_ ++ _).toSeq*) -> ns.reduce(_ ++ _)
      }
    }

  def choiceʹ(using Bindings, Duplications, Int): Parser[(+, Names)] =
    opt( "("~>choice<~")" ) ^^ { _.getOrElse(∅() -> Names()) }

  def parallel(using Bindings, Duplications, Int): Parser[(∥, Names)] =
    scale >> { scaling =>
      val scalingʹ = scaling.abs
      given Int = if scalingʹ == 1 then summon[Int] else scalingʹ
      rep1sep(sequential, "|") ^^ { _.unzip match
        case (it, ns) =>
          if scalingʹ == 0
          then
            ∥(-1, `.`(∅())) -> Names()
          else if _settings.scaling && emitter.canScale
          then
            ∥(scaling, it*) -> ns.reduce(_ ++ _)
          else
            ∥(-1, List.fill(scalingʹ)(it).reduce(_ ++ _).toSeq*) -> ns.reduce(_ ++ _)
      }
    }

  def sequential(using bindings: Bindings)(using Duplications, Int): Parser[(`.`, Names)] =
    given Bindings = Bindings(bindings)
    prefixes ~ ( leaf | choiceʹ ) ^^ {
      case (it, (bound, free)) ~ (end, freeʹ) =>
        bindings ++= cleaned
        `.`(end, it*) -> (free ++ (freeʹ &~ bound))
    }

  def prefixes(using Bindings, Int): Parser[(List[Pre], (Names, Names))] =
    rep(prefix) ^^ { _.unzip match
      case (it, _2) => _2.unzip match
        case (bs, names) =>
          val free = Names()
          names
            .zipWithIndex
            .foreach { (ns, i) =>
              val bound = bs
                .take(i)
                .reduceOption(_ ++ _)
                .getOrElse(Names())
              free ++= ns -- bound
            }
          val bound = bs.reduceOption(_ ++ _).getOrElse(Names())
          it -> (bound, free)
    }

  def prefix(using Bindings, Int): Parser[(Pre, (Names, Names))] =
    "ν"~>"("~>names<~")" ^^ { // restriction
      case it if !it.forall(_._1.isSymbol) =>
        throw PrefixChannelsParsingException(it.filterNot(_._1.isSymbol).map(_._1)*)
      case it => it.unzip match
        case (λs, bs) =>
          val bound = bs.reduce(_ ++ _)
          BindingOccurrence(bound)
          ν(λs.map(_.asSymbol.name)*) -> (bound, Names())
    } |
    μ<~"." ^^ {
      case it @ (_, (bound, free)) =>
        PendingOccurrence(free)
        BindingOccurrence(bound)
        it
    }

  def leaf(using Bindings, Duplications, Int): Parser[(-, Names)] =
    "["~condition~"]"~choice ^^ { // (mis)match
      case _ ~ cond ~ _ ~ t =>
        ?:(cond._1, t._1, None) -> (cond._2 ++ t._2)
    } |
    "if"~condition~"then"~choice~"else"~choice ^^ { // if then else
      case _ ~ cond ~ _ ~ t ~ _ ~ f =>
        ?:(cond._1, t._1, Some(f._1)) -> (cond._2 ++ (t._2 ++ f._2))
    } |
    condition~"?"~choice~":"~choice ^^ { // Elvis operator
      case cond ~ _ ~ t ~ _ ~ f =>
        ?:(cond._1, t._1, Some(f._1)) -> (cond._2 ++ (t._2 ++ f._2))
    } |
    ("!"|"¡") ~ scale >> { // [guarded] replication
      case lin ~ parallelism =>
        var parallelismʹ = if parallelism == -1 then _settings.replication._1 else parallelism
        if parallelismʹ.abs == 1 && (_settings.replication._2 || lin == "¡" ) && emitter.featuresLinearReplication then parallelismʹ = Int.MinValue
        parallelismʹ = if parallelismʹ < 2 || !(_settings.replication._2 || lin == "¡" ) || !emitter.featuresLinearReplication then parallelismʹ else -parallelismʹ
        opt( pace ) ~ opt( "."~>μ<~"." ) >> {
          case _ ~ Some((π(λ(ch: Symbol), _, Some(cons), _, _), _)) if cons.nonEmpty && cons != "ν" =>
            throw ConsGuardParsingException(cons, ch.name)
          case pace ~ Some(π @ (π(λ(ch: Symbol), λ(par: Symbol), Some(cons), _, _), _)) =>
            if ch == par
            then
            if emitter.hasReplicationInputGuardFlaw(parallelismʹ)
            then
              warn(throw GuardParsingException(ch.name, cons.isEmpty))
            val (bound, freeʹ) = π._2
            PendingOccurrence(freeʹ)
            BindingOccurrence(bound)
            choice ^^ {
              case (sum, free) =>
                val πʹ: π = {
                  π._1 match
                    case it: π =>
                      it.copy()('!' + it.υidυ)
                }
                `!`(parallelismʹ, pace, Some(πʹ), sum) -> (freeʹ ++ (free &~ bound))
            }
          case pace ~ Some(μ) =>
            val (_, freeʹ) = μ._2
            PendingOccurrence(freeʹ)
            choice ^^ {
              case (sum, free) =>
                val μʹ: μ = {
                  μ._1 match
                    case it: π =>
                      it.copy()('!' + it.υidυ)
                    case it: τ =>
                      it.copy()('!' + it.υidυ)
                }
                `!`(parallelismʹ, pace, Some(μʹ), sum) -> (freeʹ ++ free)
            }
          case pace ~ _ =>
            choice ^^ {
              case (sum, free) =>
                `!`(parallelismʹ, pace, None, sum) -> free
            }
        }
    } |
    capital ^^ {
      case it @ (_, free) =>
        PendingOccurrence(free)
        it
    } |
    invocation() ^^ {
      case it @ (_, free) =>
        PendingOccurrence(free)
        it
    } |
    instantiation

  def capital: Parser[(`{}`, Names)]

  def instantiation(using Bindings, Duplications, Int): Parser[(`⟦⟧`, Names)]

  def condition(using Bindings): Parser[(((λ, λ), Boolean), Names)] = "("~>condition<~")" |
    name~("="|"≠")~name ^^ {
      case (lhs, free_lhs) ~ mismatch ~ (rhs, free_rhs) =>
        val free = free_lhs ++ free_rhs
        PendingOccurrence(free)
        (lhs -> rhs -> (mismatch != "=")) -> free
    }

  def invocation(equation: Boolean = false): Parser[(`(*)`, Names)] =
    IDENT ~ opt( "("~> names ~ opt(if equation then "*" else "") <~")" ) ^^ {
      case identifier ~ Some(params ~ _) if equation && !params.forall(_._1.isSymbol) =>
        throw EquationParamsException(identifier, params.filterNot(_._1.isSymbol).map(_._1)*)
      case "Self" ~ Some(params ~ init) =>
        val paramsʹ = if equation && init.isDefined
                      then params.map(_._1).init
                      else params.map(_._1)
        self += _code
        `(*)`("Self_" + _code, paramsʹ*) -> params.map(_._2).reduce(_ ++ _)
      case "Self" ~ _ =>
        self += _code
        `(*)`("Self_" + _code) -> Names()
      case identifier ~ Some(params ~ init) =>
        val paramsʹ = if equation && init.isDefined
                      then params.map(_._1).init
                      else params.map(_._1)
        identifier match
          case s"Self_$n" if (try { n.toInt; true } catch _ => false) =>
            self += n.toInt
          case _ =>
        `(*)`(identifier, paramsʹ*) -> params.map(_._2).reduce(_ ++ _)
      case identifier ~ _ =>
        identifier match
          case s"Self_$n" if (try { n.toInt; true } catch _ => false) =>
            self += n.toInt
          case _ =>
        `(*)`(identifier) -> Names()
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

  type Bind = (`(*)`, +)

  export Pre.*
  export AST.*

  enum Pre:

    case ν(names: String*) // forcibly

    case τ(override val rate: Option[Any],
           code: Option[Code])(id: => String)
        extends Pre with Act(() => id)

    case π(channel: λ,
           name: λ,
           polarity: Option[String],
           override val rate: Option[Any],
           code: Option[Code])(id: => String)
        extends Pre with Act(() => id)

    override def toString: String = this match
      case ν(names*) => names.mkString("ν(", ", ", ")")
      case π(channel, name, polarity, _, _) =>
        if polarity.isDefined
        then
          if polarity.get != "ν"
          then "" + channel + s"${polarity.get}(" + name + ")."
          else "" + channel + "<ν" + name + ">."
        else "" + channel + "<" + name + ">."
      case _ => "τ."

  enum AST:

    case +(scaling: Int, choices: AST.∥ *) extends AST with Sum

    case ∥(scaling: Int, components: AST.`.`*)

    case `.`(end: AST.+ | -, prefixes: Pre*)

    case ?:(cond: ((λ, λ), Boolean), t: AST.+, f: Option[AST.+])

    case !(parallelism: Int,
           pace: Option[(Long, String)],
           guard: Option[μ],
           sum: AST.+)

    case `⟦⟧`(definition: Definition,
              sum: AST.+,
              xid: String = null,
              pointers: List[Symbol] = Nil)

    case `{}`(identifier: String,
              pointers: List[Symbol],
              agent: Boolean = false,
              params: λ*)

    case `(*)`(identifier: String,
               params: λ*)

    override def toString: String = this match
      case ∅() => "()"
      case +(-1, choices*) => choices.mkString(" + ")
      case +(sc, choices*) => sc + " * " + choices.mkString(" + ")

      case ∥(-1, components*) => components.mkString(" | ")
      case ∥(sc, components*) => sc + " * " + components.mkString(" | ")

      case `.`(∅()) => "()"
      case `.`(∅(), prefixes*) => prefixes.mkString(" ") + " ()"
      case `.`(end: +, prefixes*) =>
        prefixes.mkString(" ") + (if prefixes.isEmpty then "" else " ") + "(" + end + ")"
      case `.`(end, prefixes*) =>
        prefixes.mkString(" ") + (if prefixes.isEmpty then "" else " ") + end

      case ?:(cond, t, f) =>
        val test = "" + cond._1._1 + (if cond._2 then " ≠ " else " = ") + cond._1._2
        if f.isEmpty
        then
          "[ " + test + " ] " + t
        else
          "if " + test + " then " + t + " else " + f.get

      case !(-1, _, guard, sum) => "!" + guard.map("." + _).getOrElse("") + sum

      case !(parallelism, _, guard, sum) => s"!$parallelism*" + guard.map("." + _).getOrElse("") + sum

      case `⟦⟧`(Definition(code, term, constants, variables, _), sum, _, pointers) =>
        val assignment = if (variables.isEmpty)
                         then
                           ""
                         else {
                           (variables zip pointers).map { (l, r) => s"${l.name} = ${r.name}" }
                         ++ variables.drop(pointers.size).map(_.name)
                         }.mkString("{", ", ", "}")
        if constants.isEmpty
        then
          s"""${Definition(code, term)}$assignment = $sum"""
        else
          s"""${Definition(code, term)}${constants.map(_.name).mkString("(", ", ", ")")}$assignment = $sum"""

      case `{}`(identifier, pointers, agent, params*) =>
        val ps = if agent then params.mkString("(", ", ", ")") else ""
        s"""$identifier$ps{${pointers.map(_.name).mkString(", ")}}"""

      case `(*)`(identifier, params*) =>
        val args = params.map(_.toTerm).toList
        Term.Apply(Term.Name(identifier), Term.ArgClause(args)).toString

  given `_+_`: {} with
    extension (self: +)
      def copy(scaling: Int = self.scaling,
               choices: Seq[∥] = self.choices): + =
        `+`(scaling, choices*)

  given `_∥_`: {} with
    extension (self: ∥)
      def copy(scaling: Int = self.scaling,
               components: Seq[`.`] = self.components): ∥ =
        ∥(scaling, components*)

  given `_._`: {} with
    extension (self: `.`)
      def copy(end: + | - = self.end,
               prefixes: Seq[Pre] = self.prefixes): `.` =
        `.`(end, prefixes*)

  given `_{}_`: {} with
    extension (self: `{}`)
      def copy(identifier: String = self.identifier,
               pointers: List[Symbol] = self.pointers,
               agent: Boolean = self.agent,
               params: Seq[λ] = self.params): `{}` =
        `{}`(identifier, pointers, agent, params*)

  given `_(*)_`: {} with
    extension (self: `(*)`)
      def copy(identifier: String = self.identifier,
               params: Seq[λ] = self.params): `(*)` =
        `(*)`(identifier, params*)

  object ∅ :
    def apply(): + = `+`(-1)
    def unapply(self: AST): Boolean = self match
      case sum: + => sum.isVoid
      case _ => false

  case class λ(`val`: Any)(using val `type`: Option[(Type, Option[Type])] = None):
    val isSymbol: Boolean = `val`.isInstanceOf[Symbol]
    def asSymbol: Symbol = `val`.asInstanceOf[Symbol]

    type Kind = `val`.type

    val kind: String = `val` match
      case _: Symbol => "channel name"
      case _: BigDecimal => "decimal number"
      case _: Boolean => "True False"
      case _: String => "string literal"
      case _: Term => "Scalameta Term"
      case _ => "polyadic names"

    def toTerm: Term =
      import scala.meta._
      import dialects.Scala3
      `val` match
        case it: Symbol => Term.Name(it.name)
        case it: BigDecimal => Term.Apply(Term.Name("BigDecimal"), Term.ArgClause(Lit.String(it.toString)::Nil))
        case it: Boolean => Lit.Boolean(it)
        case it: String => Lit.String(it)
        case it: Term => Expression(it)._1

    def toPat: Pat =
      import scala.meta._
      import dialects.Scala3
      `val` match
        case it: Symbol => Pat.Macro(Term.QuotedMacroExpr(Term.Name(it.name)))
        case it: BigDecimal => Lit.Double(it.toDouble)
        case it: Boolean => Lit.Boolean(it)
        case it: String => Lit.String(it)
        case it: Term => it.asInstanceOf[Pat]

    override def toString: String = `val` match
      case it: Symbol => it.name
      case it: BigDecimal => "" + it
      case it: Boolean => it.toString.capitalize
      case it: String => "\"" + it + "\""
      case it: Term => "/*" + it + "*/"
      case it: List[`λ`] => it.mkString(", ")


  // exceptions

  import Expression.ParsingException

  abstract class EquationParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class EquationParamsException(identifier: String, params: λ*)
      extends EquationParsingException(s"""The "formal" parameters (${params.mkString(", ")}) are not names in the left hand side of $identifier""")

  case class EquationFreeNamesException(identifier: String, free: Names)
      extends EquationParsingException(s"""The free names (${free.map(_.name).mkString(", ")}) in the right hand side are not formal parameters of the left hand side of $identifier""")

  case class PrefixChannelsParsingException(names: λ*)
      extends PrefixParsingException(s"""${names.mkString(", ")} are not channel names but ${names.map(_.kind).mkString(", ")}""")

  case class GuardParsingException(name: String, input: Boolean)
      extends PrefixParsingException(s"""$name is both the channel name and ${if input then "the binding parameter name in an input guard" else "the new name in a bound output guard"}""")

  case class ConsGuardParsingException(cons: String, name: String)
      extends PrefixParsingException(s"A name $name that knows how to CONS (`$cons') is used as replication guard")


  // functions

  extension (sum: +)
    def isVoid: Boolean = sum match
      case +(_) => true
      case _ => sum.choices.forall(_.components.forall { case `.`(sum: +) => sum.isVoid case _ => false })

  extension [T <: AST](ast: T)

    def foreach(g: AST => Unit)(h: PartialFunction[AST, Unit] = PartialFunction.empty): Unit =

      h.applyOrElse(ast, {

        case ∅() =>

        case +(_, choices*) =>
          choices.foreach(_.foreach(g)(h))

        case ∥(_, components*) =>
          components.foreach(_.foreach(g)(h))

        case `.`(end, _*) =>
          end.foreach(g)(h)

        case ?:(_, t, f) =>
          t.foreach(g)(h)
          f.foreach(_.foreach(g)(h))

        case !(_, _, _, sum) =>
          sum.foreach(g)(h)

        case `⟦⟧`(_, sum, _, _) =>
          sum.foreach(g)(h)

        case _ => g(ast)

      })

    def mapreduce[R](g: AST => R)(h: (R, R) => R): R =

      ast match

        case ∅() => g(ast)

        case +(_, choices*) =>
          choices.map(_.mapreduce(g)(h)).reduce(h)

        case ∥(_, components*) =>
          components.map(_.mapreduce(g)(h)).reduce(h)

        case it @ `.`(end, _*) =>
          h(g(it), end.mapreduce(g)(h))

        case it @ ?:(_, t, f) =>
          h(h(g(it), t.mapreduce(g)(h)), f.fold(g(∅()))(_.mapreduce(g)(h)))

        case it @ !(_, _, _, sum) =>
          h(g(it), sum.mapreduce(g)(h))

        case it @ `⟦⟧`(_, sum, _, _) =>
          h(g(it), sum.mapreduce(g)(h))

        case _ => g(ast)

    def map(g: AST => AST)(h: AST => AST = identity): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case it @ +(_, choices*) =>
          it.copy(choices = choices.map(_.map(g)(h)))

        case it @ ∥(_, components*) =>
          it.copy(components = components.map(_.map(g)(h)))

        case it @ `.`(end, _*) =>
          h(it.copy(end = end.map(g)(h)))

        case ?:(cond, t, f) =>
          h(?:(cond, t.map(g)(h), f.map(_.map(g)(h))))

        case it @ !(_, _, _, sum) =>
          h(it.copy(sum = sum.map(g)(h)))

        case it @ `⟦⟧`(_, sum, _, _) =>
          h(it.copy(sum = sum.map(g)(h)))

        case _ => h(ast)

    def mapʹ(g: AST => AST)(h: AST => AST): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case it @ +(_, choices*) =>
          it.copy(choices = choices.map(_.mapʹ(g)(h)))

        case it @ ∥(_, components*) =>
          it.copy(components = components.map(_.mapʹ(g)(h)))

        case it: `.` =>
          val itʹ @ `.`(end, _*) = h(it)
          itʹ.copy(end = end.mapʹ(g)(h))

        case it: ?: =>
          val itʹ @ ?:(_, t, f) = h(it)
          itʹ.copy(t = t.mapʹ(g)(h), f = f.map(_.mapʹ(g)(h)))

        case it: ! =>
          val itʹ @ !(_, _, _, sum) = h(it)
          itʹ.copy(sum = sum.mapʹ(g)(h))

        case it: `⟦⟧` =>
          val itʹ @ `⟦⟧`(_, sum, _, _) = h(it)
          itʹ.copy(sum = sum.mapʹ(g)(h))

        case _ => h(ast)

    def mapʹʹ(g: AST => AST)(h: AST => (AST, Boolean)): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case it @ +(_, choices*) =>
          it.copy(choices = choices.map(_.mapʹʹ(g)(h)))

        case it @ ∥(_, components*) =>
          it.copy(components = components.map(_.mapʹʹ(g)(h)))

        case it: `.` =>
          h(it) match
            case (itʹ @ `.`(end, _*), false) =>
              itʹ.copy(end = end.mapʹʹ(g)(h))
            case (itʹ, _) => itʹ

        case it: ?: =>
          h(it) match
            case (itʹ @ ?:(_, t, f), false) =>
              itʹ.copy(t = t.mapʹʹ(g)(h), f = f.map(_.mapʹʹ(g)(h)))
            case (itʹ, _) => itʹ

        case it: ! =>
          h(it) match
            case (itʹ @ !(_, _, _, sum), false) =>
              itʹ.copy(sum = sum.mapʹʹ(g)(h))
            case (itʹ, _) => itʹ

        case it: `⟦⟧` =>
          h(it) match
            case (itʹ @ `⟦⟧`(_, sum, _, _), false) =>
              itʹ.copy(sum = sum.mapʹʹ(g)(h))
            case (itʹ, _) => itʹ

        case _ => h(ast)._1

    def flatten: T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() =>
          ∅()

        case it @ +(_, ∥(-1|1, `.`(sum: +)), choices*) =>
          val lhs = sum.flatten
          val rhs = `+`(-1, choices*).flatten
          it.copy(choices = (lhs.choices ++ rhs.choices).filterNot(`+`(-1, _).isVoid))

        case it @ +(_, par, choices*) =>
          val lhs: + = `+`(-1, par.flatten)
          val rhs = `+`(-1, choices*).flatten
          it.copy(choices = (lhs.choices ++ rhs.choices).filterNot(`+`(-1, _).isVoid))

        case it @ ∥(_, `.`(+(-1|1, par)), components*) =>
          val lhs = par.flatten
          val rhs = ∥(-1, components*).flatten
          it.copy(components = lhs.components ++ rhs.components)

        case it @ ∥(sc, seq, components*) =>
          val lhs: ∥ = ∥(-1, seq.flatten)
          val rhs = ∥(-1, components*).flatten
          it.copy(components = lhs.components ++ rhs.components)

        case `.`(+(-1|1, ∥(-1|1, `.`(end, psr*))), psl*) =>
          `.`(end, (psl ++ psr)*).flatten

        case it @ `.`(end, _*) =>
          it.copy(end = end.flatten)

        case ?:(cond, t, f) =>
          ?:(cond, t.flatten, f.map(_.flatten))

        case !(-1, None, None, sum) =>
          sum.flatten match
            case +(-1|1, ∥(-1|1, `.`(end: !))) => end
            case it => `!`(-1, None, None, it)

        case it @ !(_, _, _, sum) =>
          it.copy(sum = sum.flatten)

        case _ => ast

    def labelʹ(using String): T =

      ast match

        case +(_, ∥(_, `.`(!(_, _, Some(_), _), _*))) =>
          ast.label("+0/-1")

        case _ =>
          ast.label("")

    def label(l: String)(using String): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      object Sum:
        inline implicit def lʹ(i: Int)(using n: Int): String = l + "+" + i + "/" + n

      object Par:
        inline implicit def lʹ(i: Int)(using n: Int): String = l + "∥" + i + "/" + n

      inline def idʹ(id: => String, ch: String, p: String, r: Any): String =
        id + "," + ch + "," + p + "," + l + "," + rateʹ(r) + "," + summon[String]

      val relabelled: Seq[Pre] => Seq[Pre] =
        _.map {
          case it: τ =>
            it.copy()(idʹ(it.id, "τ", "", it.rate.get))
          case it @ π(λ(Symbol(name)), _, None | Some("" | "ν"), rate, _) =>
            val polarity = it.polarity match { case Some("") => true case _ => false }
            it.copy()(idʹ(it.id, name, polarity.toString, rate.get))
          case it => it
        }

      inline def relabelledʹ(it: Option[μ]): Option[μ] =
        relabelled(it.toSeq).headOption.asInstanceOf[Option[μ]]

      ast match

        case ∅() => ast

        case +(sc, ∥(scʹ, it: `.`)) if !it.prefixes.exists { case Act(it) => it } =>
          `+`(sc, ∥(scʹ, it.label(l)))

        case +(sc, ∥(scʹ, it*)) =>
          import Par.*
          given Int = it.size
          `+`(sc, ∥(scʹ, it.zipWithIndex.map(_.label(_))*))

        case +(sc, it*) =>
          import Sum.*
          given Int = it.size
          `+`(sc, it.zipWithIndex.map(_.label(_))*)

        case ∥(sc, it*) =>
          ∥(sc, it.map(_.label(l))*)

        case `.`(end, it*) =>
          `.`(end.label(l), relabelled(it)*)

        case ?:(cond, t, Some(f)) =>
          import Sum.*
          given Int = 0
          ?:(cond, t.label(0), Some(f.label(1)))

        case ?:(cond, t, _) =>
          ?:(cond, t.label(l), None)

        case it @ !(_, _, guard @ Some(_), sum) =>
          it.copy(guard = relabelledʹ(guard), sum = sum.label(l))

        case it @ !(_, _, _, sum) =>
          it.copy(sum = sum.label(l))

        case it @ `⟦⟧`(_, sum, _, _) =>
          it.copy(sum = sum.label(l))

        case _ => ast
