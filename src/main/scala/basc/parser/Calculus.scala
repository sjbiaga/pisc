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

package basc
package parser

import scala.meta.{ Pat, Term, Type }

import scala.util.parsing.input.{ NoPosition, Position }

import emitter.shared.Meta.rateʹ

import Expression.Code
import BioAmbients.*
import Calculus.*
import Encoding.*
import scala.util.parsing.combinator.basc.parser.Expansion.Duplications


abstract class Calculus extends BioAmbients:

  def equation(using Duplications): Parser[Bind] =
    positioned(invocation(true)) >> {
      case bind: `(*)` if _settings.exclude =>
        ".*".r ^^ { _ => bind -> ∅() }
      case bind: `(*)` =>
        val bound = bind.free
        _code = -1
        _directive = None
        given Bindings = Bindings() ++ bound.map(_ -> Occurrence(None, pos()))
        given Int = 1
        "="~> positioned(choice) ^^ { _sum =>
          val sum: + =
            _sum.flatten match {
              case ∅() if bind match { case `(*)`("Main") => true case _ => false } =>
                val pos = bind.pos
                `+`(-1, ∥(-1, `.`(∅(pos), τ(Some(-1L), None)(sπ_id).setPos(pos)).setPos(pos)).setPos(pos)).setPos(pos)
              case it => it
            }
          val free = _sum.free ++ sum.capitals
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

  def choice(using Bindings, Duplications, Int): Parser[+] =
    scale >> { scaling =>
      val scalingʹ = scaling.abs
      given Int = if scalingʹ == 1 then summon[Int] else scalingʹ
      rep1sep(positioned(parallel), "+") ^^ { it =>
        val ns = it.map(_.free)
        if scalingʹ == 0
        then
          ∅()
        else if _settings.scaling && emitter.canScale
        then
          `+`(scaling, it*).free = ns.reduce(_ ++ _)
        else
          `+`(-1, List.fill(scalingʹ)(it).reduce(_ ++ _).toSeq*).free = ns.reduce(_ ++ _)
      }
    }

  def choiceʹ(using Bindings, Duplications, Int): Parser[+] =
    opt( "("~>positioned(choice)<~")" ) ^^ (_.getOrElse(∅()))

  def parallel(using Bindings, Duplications, Int): Parser[∥] =
    scale >> { scaling =>
      val scalingʹ = scaling.abs
      given Int = if scalingʹ == 1 then summon[Int] else scalingʹ
      rep1sep(positioned(sequential), "|") ^^ { it =>
        val ns = it.map(_.free)
        if scalingʹ == 0
        then
          val pos = it.head.pos
          ∥(-1, `.`(∅(pos)).setPos(pos))
        else if _settings.scaling && emitter.canScale
        then
          ∥(scaling, it*).free = ns.reduce(_ ++ _)
        else
          ∥(-1, List.fill(scalingʹ)(it).reduce(_ ++ _).toSeq*).free = ns.reduce(_ ++ _)
      }
  }

  def sequential(using bindings: Bindings)(using Duplications, Int): Parser[`.`] =
    given Bindings = Bindings(bindings)
    prefixes ~ ( positioned(leaf) | positioned(choiceʹ) ) ^^ {
      case (it, (bound, free)) ~ (end: (+ | -)) =>
        bindings ++= cleaned
        `.`(end, it*).free = free ++ (end.free &~ bound)
    }

  def prefixes(using Bindings, Int): Parser[(List[Pre], (Names, Names))] =
    rep(positioned(prefix)) ^^ { it =>
      val (bs, names) = it.map(_.bound) -> it.map(_.free)
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

  def prefix(using Bindings, Int): Parser[Pre] =
    positioned {
      "ν"~>"("~>names<~")" ^^ { // restriction
        case it if !it.forall(_._1.isSymbol) =>
          throw PrefixChannelsParsingException(it.filterNot(_._1.isSymbol).map(_._1)*)
        case it => it.unzip match
          case (λs, bs) =>
            val bound = bs.reduce(_ ++ _)
            BindingOccurrence(bound)
            ν(λs.map(_.asSymbol.name)*).bound = bound
      }
    } |
    positioned(μ)<~"." ^^ { it =>
      PendingOccurrence(it.free)
      BindingOccurrence(it.bound)
      it
    } |
    positioned(ζ)<~"." ^^ { it =>
      PendingOccurrence(it.free)
      it
    }

  def leaf(using Bindings, Duplications, Int): Parser[AST] =
    "["~condition~"]"~positioned(choice) ^^ { // (mis)match
      case _ ~ cond ~ _ ~ t =>
        ?:(cond._1, t, None).free = cond._2 ++ t.free
    } |
    "if"~condition~"then"~positioned(choice)~"else"~positioned(choice) ^^ { // if then else
      case _ ~ cond ~ _ ~ t ~ _ ~ f =>
        ?:(cond._1, t, Some(f)).free = (cond._2 ++ (t.free ++ f.free))
    } |
    condition~"?"~positioned(choice)~":"~positioned(choice) ^^ { // Elvis operator
      case cond ~ _ ~ t ~ _ ~ f =>
        ?:(cond._1, t, Some(f)).free = (cond._2 ++ (t.free ++ f.free))
    } |
    ("!"|"¡") ~ scale >> { // [guarded] replication
      case lin ~ parallelism =>
        var parallelismʹ = if parallelism == -1 then _settings.replication._1 else parallelism
        if parallelismʹ.abs == 1 && (_settings.replication._2 || lin == "¡" ) && emitter.featuresLinearReplication then parallelismʹ = Int.MinValue
        parallelismʹ = if parallelismʹ < 2 || !(_settings.replication._2 || lin == "¡" ) || !emitter.featuresLinearReplication then parallelismʹ else -parallelismʹ
        opt( pace ) ~ opt( "."~>(positioned(μ) | positioned(ζ))<~"." ) >> { // [guarded] replication
          case _ ~ Some(π(_, λ(ch: Symbol), _, Some(cons), _, _)) if cons.nonEmpty && cons != "ν" =>
            throw ConsGuardParsingException(cons, ch.name)
          case pace ~ Some(π @ π(_, λ(ch: Symbol), λ(par: Symbol), Some(cons), _, _)) =>
            if ch == par
            then
              if emitter.hasReplicationInputGuardFlaw(parallelismʹ)
              then
                warn(throw GuardParsingException(ch.name, cons.isEmpty))
            val (bound, free) = π.bound -> π.free
            PendingOccurrence(free)
            BindingOccurrence(bound)
            positioned(choice) ^^ { sum =>
              val πʹ = π.cc()('!' + π.υidυ)
              `!`(parallelismʹ, pace, Some(πʹ), sum).free = free ++ (sum.free &~ bound)
            }
          case pace ~ Some(μ) =>
            val free = μ.free
            PendingOccurrence(free)
            positioned(choice) ^^ { sum =>
              val μʹ: μ | ζ = {
                μ match
                  case it: π =>
                    it.cc()('!' + it.υidυ)
                  case it: τ =>
                    it.cc()('!' + it.υidυ)
                  case it: ζ =>
                    it.cc()('!' + it.υidυ)
              }
              `!`(parallelismʹ, pace, Some(μʹ), sum).free = free ++ sum.free
            }
          case pace ~ _ =>
            positioned(choice) ^^ { sum =>
              `!`(parallelismʹ, pace, None, sum).free = sum.free
            }
        }
    } |
    positioned {
      opt(stringLiteral) ~ ("["~>positioned(choice)<~"]") ^^ { // ambient
        case Some(label) ~ _ if label.contains(',') =>
          throw AmbientLabelParsingException(label)
        case label ~ sum =>
          val labelʹ = label.map(_.stripPrefix("\"").stripSuffix("\""))
          `[]`(labelʹ, sum).free = sum.free
      }
    } |
    positioned(capital) ^^ { it =>
      PendingOccurrence(it.free)
      it
    } |
    positioned(invocation()) ^^ { it =>
      PendingOccurrence(it.free)
      it
    } |
    positioned(instantiation)

  def capital: Parser[`{}`]

  def instantiation(using Bindings, Duplications, Int): Parser[AST]

  def condition(using Bindings): Parser[(((λ, λ), Boolean), Names)] = "("~>condition<~")" |
    name~("="|"≠")~name ^^ {
      case (lhs, free_lhs) ~ mismatch ~ (rhs, free_rhs) =>
        val free = free_lhs ++ free_rhs
        PendingOccurrence(free)
        (lhs -> rhs -> (mismatch != "=")) -> free
    }

  def invocation(equation: Boolean = false): Parser[`(*)`] =
    IDENT ~ opt( "("~> names ~ opt(if equation then "*" else "") <~")" ) ^^ {
      case identifier ~ Some(params ~ _) if equation && !params.forall(_._1.isSymbol) =>
        throw EquationParamsException(identifier, params.filterNot(_._1.isSymbol).map(_._1)*)
      case "Self" ~ Some(params ~ init) =>
        val paramsʹ = if equation && init.isDefined
                      then params.map(_._1).init
                      else params.map(_._1)
        self += _code
        `(*)`("Self_" + _code, paramsʹ*).free = params.map(_._2).reduce(_ ++ _)
      case "Self" ~ _ =>
        self += _code
        `(*)`("Self_" + _code)
      case identifier ~ Some(params ~ init) =>
        val paramsʹ = if equation && init.isDefined
                      then params.map(_._1).init
                      else params.map(_._1)
        identifier match
          case s"Self_$n" if (try { n.toInt; true } catch _ => false) =>
            self += n.toInt
          case _ =>
        `(*)`(identifier, paramsʹ*).free = params.map(_._2).reduce(_ ++ _)
      case identifier ~ _ =>
        identifier match
          case s"Self_$n" if (try { n.toInt; true } catch _ => false) =>
            self += n.toInt
          case _ =>
        `(*)`(identifier)
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

  trait Positional extends scala.util.parsing.input.Positional:
    val lc: Int => (Int, Int)

  type Bind = (`(*)`, +)

  export Pre.*
  export AST.*

  enum Pre extends Positional with Free with Bound:

    case ν(names: String*) // forcibly
          (using override val lc: Int => (Int, Int))

    case τ(override val rate: Option[Any],
           code: Option[Code])(id: => String)
          (using override val lc: Int => (Int, Int))
        extends Pre with Act(() => id)

    case π(dir: `$`,
           channel: λ,
           name: λ,
           polarity: Option[String],
           override val rate: Option[Any],
           code: Option[Code])(id: => String)
          (using override val lc: Int => (Int, Int))
        extends Pre with Act(() => id)

    case ζ(cap: Cap,
           name: String,
           polarity: Boolean,
           override val rate: Option[Any],
           code: Option[Code])(id: => String)
          (using override val lc: Int => (Int, Int))
        extends Pre with Act(() => id)

    override def toString: String = this match
      case ν(names*) => names.mkString("ν(", ", ", ")")
      case π(dir, channel, name, polarity, _, _) =>
        if polarity.isDefined
        then
          if polarity.get != "ν"
          then "" + channel + s"${polarity.get} ? {" + name + "}."
          else s"$dir " + channel + " ! {ν" + name + "}."
        else s"$dir " + channel + " ! {" + name + "}."
      case ζ(cap, name, _, _, _) =>
        "" + cap + " " + name + "."
      case _ => "τ."

  given [T <: Pre]: Conversion[Pre, T] = _.asInstanceOf[T]

  given `_ν_`: {} with
    extension (self: ν)
      def cc(names: Seq[String] = self.names): + =
        (ν(names*)(using self.lc).setPos(self.pos).bound = self.bound).free = self.free

  given `_τ_`: {} with
    extension (self: τ)
      def cc(rate: Option[Any] = self.rate,
             code: Option[Code] = self.code)(id: => String = self.id): τ =
        (τ(rate, code)(id)(using self.lc).setPos(self.pos).bound = self.bound).free = self.free

  given `_π_`: {} with
    extension (self: π)
      def cc(dir: `$` = self.dir,
             channel: λ = self.channel,
             name: λ = self.name,
             polarity: Option[String] = self.polarity,
             rate: Option[Any] = self.rate,
             code: Option[Code] = self.code)(id: => String = self.id): π =
        (π(dir, channel, name, polarity, rate, code)(id)(using self.lc).setPos(self.pos).bound = self.bound).free = self.free

  given `_ζ_`: {} with
    extension (self: ζ)
      def cc(cap: Cap = self.cap,
             name: String = self.name,
             polarity: Boolean = self.polarity,
             rate: Option[Any] = self.rate,
             code: Option[Code] = self.code)(id: => String = self.id): ζ =
        (ζ(cap, name, polarity, rate, code)(id)(using self.lc).setPos(self.pos).bound = self.bound).free = self.free

  enum AST extends Positional with Free:

    case +(scaling: Int, choices: AST.∥ *)
          (using override val lc: Int => (Int, Int))
        extends AST with Sum

    case ∥(scaling: Int, components: AST.`.`*)
          (using override val lc: Int => (Int, Int))

    case `.`(end: AST.+ | -, prefixes: Pre*)
            (using override val lc: Int => (Int, Int))

    case ?:(cond: ((λ, λ), Boolean), t: AST.+, f: Option[AST.+])
           (using override val lc: Int => (Int, Int))

    case !(parallelism: Int,
           pace: Option[(Long, String)],
           guard: Option[μ | ζ],
           sum: AST.+)
          (using override val lc: Int => (Int, Int))

    case `[]`(label: Option[String], sum: AST.+)
             (using override val lc: Int => (Int, Int))

    case `⟦⟧`(definition: Definition,
              sum: AST.+,
              xid: String = null,
              pointers: List[Symbol] = Nil)
             (using override val lc: Int => (Int, Int))

    case `{}`(identifier: String,
              pointers: List[Symbol],
              agent: Boolean = false,
              params: λ*)
             (using override val lc: Int => (Int, Int))

    case `(*)`(identifier: String,
               params: λ*)
              (using override val lc: Int => (Int, Int))

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

      case !(-1, _, guard, sum) =>
        "!" + guard.map("." + _).getOrElse("") + sum

      case !(parallelism, _, guard, sum) if parallelism < -1 =>
        s"¡${-(parallelism%Int.MaxValue)}*" + guard.map("." + _).getOrElse("") + sum

      case !(parallelism, _, guard, sum) =>
        s"!$parallelism*" + guard.map("." + _).getOrElse("") + sum

      case `[]`(label, sum) =>
        label.getOrElse("") + "[ " + sum + " ]"

      case `⟦⟧`(_, sum, _, _) =>
        sum.toString

      case `{}`(identifier, pointers, agent, params*) =>
        val ps = if agent then params.mkString("(", ", ", ")") else ""
        s"""$identifier$ps{${pointers.map(_.name).mkString(", ")}}"""

      case `(*)`(identifier, params*) =>
        val args = params.map(_.toTerm).toList
        Term.Apply(Term.Name(identifier), Term.ArgClause(args)).toString

  given [T <: AST]: Conversion[AST, T] = _.asInstanceOf[T]

  given `_+_`: {} with
    extension (self: +)
      def cc(scaling: Int = self.scaling,
             choices: Seq[∥] = self.choices): + =
        `+`(scaling, choices*)(using self.lc).setPos(self.pos).free = self.free

  given `_∥_`: {} with
    extension (self: ∥)
      def cc(scaling: Int = self.scaling,
             components: Seq[`.`] = self.components): ∥ =
        ∥(scaling, components*)(using self.lc).setPos(self.pos).free = self.free

  given `_._`: {} with
    extension (self: `.`)
      def cc(end: + | - = self.end,
             prefixes: Seq[Pre] = self.prefixes): `.` =
        `.`(end, prefixes*)(using self.lc).setPos(self.pos).free = self.free

  given `_?:_`: {} with
    extension (self: ?:)
      def cc(cond: ((λ, λ), Boolean) = self.cond,
             t: AST.+ = self.t,
             f: Option[AST.+] = self.f): ?: =
        `?:`(cond, t, f)(using self.lc).setPos(self.pos).free = self.free

  given `_!_`: {} with
    extension (self: !)
      def cc(parallelism: Int = self.parallelism,
             pace: Option[(Long, String)] = self.pace,
             guard: Option[μ | ζ] = self.guard,
             sum: AST.+ = self.sum): ! =
        `!`(parallelism, pace, guard, sum)(using self.lc).setPos(self.pos).free = self.free

  given `_[]_`: {} with
    extension (self: `[]`)
      def cc(label: Option[String] = self.label,
             sum: AST.+ = self.sum): `[]` =
        `[]`(label, sum)(using self.lc).setPos(self.pos).free = self.free

  given `_⟦⟧_`: {} with
    extension (self: `⟦⟧`)
      def cc(definition: Definition = self.definition,
             sum: AST.+ = self.sum,
             xid: String = self.xid,
             pointers: List[Symbol] = self.pointers): `⟦⟧` =
        `⟦⟧`(definition, sum, xid, pointers)(using self.lc).setPos(self.pos).free = self.free

  given `_{}_`: {} with
    extension (self: `{}`)
      def cc(identifier: String = self.identifier,
             pointers: List[Symbol] = self.pointers,
             agent: Boolean = self.agent,
             params: Seq[λ] = self.params): `{}` =
        `{}`(identifier, pointers, agent, params*)(using self.lc).setPos(self.pos).free = self.free

  given `_(*)_`: {} with
    extension (self: `(*)`)
      def cc(identifier: String = self.identifier,
             params: Seq[λ] = self.params): `(*)` =
        `(*)`(identifier, params*)(using self.lc).setPos(self.pos).free = self.free

  object ∅ :
    def apply(pos: Position = NoPosition)(using Int => (Int, Int)): + = `+`(-1).setPos(pos)
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

  case class AmbientLabelParsingException(label: String)
      extends ParsingException(s"An ambient label $label contains commas")


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

        case `[]`(_, sum) =>
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

        case it @ ?:(_, t, Some(f)) =>
          h(h(g(it), t.mapreduce(g)(h)), f.mapreduce(g)(h))

        case it @ ?:(_, t, _) =>
          h(g(it), t.mapreduce(g)(h))

        case it @ !(_, _, _, sum) =>
          h(g(it), sum.mapreduce(g)(h))

        case it @ `[]`(_, sum) =>
          h(g(it), sum.mapreduce(g)(h))

        case it @ `⟦⟧`(_, sum, _, _) =>
          h(g(it), sum.mapreduce(g)(h))

        case _ => g(ast)

    def map(g: AST => AST)(h: AST => AST = identity): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case it @ +(_, choices*) =>
          it.cc(choices = choices.map(_.map(g)(h)))

        case it @ ∥(_, components*) =>
          it.cc(components = components.map(_.map(g)(h)))

        case it @ `.`(end, _*) =>
          h(it.cc(end = end.map(g)(h)))

        case it @ ?:(_, t, f) =>
          h(it.cc(t = t.map(g)(h), f = f.map(_.map(g)(h))))

        case it @ !(_, _, _, sum) =>
          h(it.cc(sum = sum.map(g)(h)))

        case it @ `[]`(_, sum) =>
          h(it.cc(sum = sum.map(g)(h)))

        case it @ `⟦⟧`(_, sum, _, _) =>
          h(it.cc(sum = sum.map(g)(h)))

        case _ => h(ast)

    def mapʹ(g: AST => AST)(h: AST => AST): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case it @ +(_, choices*) =>
          it.cc(choices = choices.map(_.mapʹ(g)(h)))

        case it @ ∥(_, components*) =>
          it.cc(components = components.map(_.mapʹ(g)(h)))

        case it: `.` =>
          val itʹ @ `.`(end, _*) = h(it)
          itʹ.cc(end = end.mapʹ(g)(h))

        case it: ?: =>
          val itʹ @ ?:(_, t, f) = h(it)
          itʹ.cc(t = t.mapʹ(g)(h), f = f.map(_.mapʹ(g)(h)))

        case it: ! =>
          val itʹ @ !(_, _, _, sum) = h(it)
          itʹ.cc(sum = sum.mapʹ(g)(h))

        case it: `[]` =>
          val itʹ @ `[]`(_, sum) = h(it)
          itʹ.cc(sum = sum.mapʹ(g)(h))

        case it: `⟦⟧` =>
          val itʹ @ `⟦⟧`(_, sum, _, _) = h(it)
          itʹ.cc(sum = sum.mapʹ(g)(h))

        case _ => h(ast)

    def mapʹʹ(g: AST => AST)(h: AST => (AST, Boolean)): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case it @ +(_, choices*) =>
          it.cc(choices = choices.map(_.mapʹʹ(g)(h)))

        case it @ ∥(_, components*) =>
          it.cc(components = components.map(_.mapʹʹ(g)(h)))

        case it: `.` =>
          h(it) match
            case (itʹ @ `.`(end, _*), false) =>
              itʹ.cc(end = end.mapʹʹ(g)(h))
            case (itʹ, _) => itʹ

        case it: ?: =>
          h(it) match
            case (itʹ @ ?:(_, t, f), false) =>
              itʹ.cc(t = t.mapʹʹ(g)(h), f = f.map(_.mapʹʹ(g)(h)))
            case (itʹ, _) => itʹ

        case it: ! =>
          h(it) match
            case (itʹ @ !(_, _, _, sum), false) =>
              itʹ.cc(sum = sum.mapʹʹ(g)(h))
            case (itʹ, _) => itʹ

        case it: `[]` =>
          h(it) match
            case (itʹ @ `[]`(_, sum), false) =>
              itʹ.cc(sum = sum.mapʹʹ(g)(h))
            case (itʹ, _) => itʹ

        case it: `⟦⟧` =>
          h(it) match
            case (itʹ @ `⟦⟧`(_, sum, _, _), false) =>
              itʹ.cc(sum = sum.mapʹʹ(g)(h))
            case (itʹ, _) => itʹ

        case _ => h(ast)._1

    def flatten: T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      given (Int => (Int, Int)) = ast.lc

      ast match

        case it @ ∅() =>
          ∅(it.pos)

        case it @ +(_, ∥(-1|1, `.`(sum: +)), choices*) =>
          val lhs = sum.flatten
          val rhs = `+`(-1, choices*).flatten
          it.cc(choices = (lhs.choices ++ rhs.choices).filterNot(`+`(-1, _).isVoid))

        case it @ +(_, par, choices*) =>
          val lhs = `+`(-1, par.flatten)
          val rhs = `+`(-1, choices*).flatten
          it.cc(choices = (lhs.choices ++ rhs.choices).filterNot(`+`(-1, _).isVoid))

        case it @ ∥(_, `.`(+(-1|1, par)), components*) =>
          val lhs = par.flatten
          val rhs = ∥(-1, components*).flatten
          it.cc(components = lhs.components ++ rhs.components)

        case it @ ∥(_, seq, components*) =>
          val lhs: ∥ = ∥(-1, seq.flatten)
          val rhs = ∥(-1, components*).flatten
          it.cc(components = lhs.components ++ rhs.components)

        case it @ `.`(+(-1|1, ∥(-1|1, `.`(end, psr*))), psl*) =>
          it.cc(end = end, prefixes = psl ++ psr).flatten

        case it @ `.`(end, _*) =>
          it.cc(end = end.flatten)

        case it @ ?:(_, t, f) =>
          it.cc(t = t.flatten, f = f.map(_.flatten))

        case it @ !(-1, None, None, sum) =>
          sum.flatten match
            case +(-1|1, ∥(-1|1, `.`(end: !))) => end
            case sumʹ => it.cc(sum = sumʹ)

        case it @ !(_, _, _, sum) =>
          it.cc(sum = sum.flatten)

        case it @ `[]`(_, sum) =>
          it.cc(sum = sum.flatten)

        case _ => ast

    def labelʹ(using String)(using patch: Boolean = false): T =

      ast match

        case +(_, ∥(_, `.`(!(_, _, Some(_), _), it*))) if !it.exists { case Act(it) => it } =>
          ast.label("+0/0")

        case _ =>
          ast.label("")

    def label(l: String)(using agent: String, patch: Boolean): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      object Sum:
        inline implicit def lʹ(i: Int)(using n: Int): String = l + "+" + i + "/" + n

      object Par:
        inline implicit def lʹ(i: Int)(using n: Int): String = l + "∥" + i + "/" + n

      inline def idʹ(it: μ | ζ, ch: String, p: String, r: Any, dc: String): String =
        val (line, col) = it.lc(it.pos.column-1)
        val lʹ = s"$l@$line.$col"
        it.υidυ + "," + ch + "," + p + "," + lʹ + "," + rateʹ(r) + "," + agent + "," + dc

      val relabelled: Seq[Pre] => Seq[Pre] =
        _.map {
          case it @ τ(Some(0L), _) =>
            it.cc(rate = Some(-1L))(idʹ(it, "τ", "", -1L, "local"))
          case it if patch => it
          case it: τ =>
            it.cc()(idʹ(it, "τ", "", it.rate.get, "local"))
          case it @ π(dir, λ(Symbol(name)), _, None | Some("" | "ν"), rate, _) =>
            val polarity = it.polarity match { case Some("") => true case _ => false }
            it.cc()(idʹ(it, name, polarity.toString, rate.get, dir.toString))
          case it @ ζ(cap, name, polarity, rate, _) =>
            it.cc()(idʹ(it, name, polarity.toString, rate.get, cap.toString))
          case it => it
        }

      inline def relabelledʹ(it: Some[μ | ζ]): Option[μ | ζ] =
        relabelled(it.toSeq).headOption.asInstanceOf[Option[μ | ζ]]

      ast match

        case ∅() => ast

        case sum @ +(_, par @ ∥(_, it: `.`)) if !it.prefixes.exists { case Act(it) => it } =>
          sum.cc(choices = Seq(par.cc(components = Seq(it.label(l)))))

        case sum @ +(_, par @ ∥(_, it*)) =>
          import Par.*
          given Int = it.size
          sum.cc(choices = Seq(par.cc(components = it.zipWithIndex.map(_.label(_)))))

        case sum @ +(_, it*) =>
          import Sum.*
          given Int = it.size
          sum.cc(choices = it.zipWithIndex.map(_.label(_)))

        case par @ ∥(_, it*) =>
          par.cc(components = it.map(_.label(l)))

        case seq @ `.`(end, it*) =>
          seq.cc(end = end.label(l), prefixes = relabelled(it))

        case it @ ?:(cond, t, Some(f)) =>
          import Sum.*
          given Int = 1
          it.cc(t = t.label(0), f = Some(f.label(1)))

        case it @ ?:(_, t, _) =>
          it.cc(t = t.label(l))

        case it @ !(_, _, guard @ Some(_), sum) =>
          it.cc(guard = relabelledʹ(guard), sum = sum.label(l))

        case it @ !(_, _, _, sum) =>
          it.cc(sum = sum.label(l))

        case it @ `[]`(_, sum) =>
          it.cc(sum = sum.label(l))

        case it @ `⟦⟧`(_, sum, _, _) =>
          it.cc(sum = sum.label(l))

        case _ => ast
