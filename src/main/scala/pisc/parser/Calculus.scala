/*
 * Copyright (c) 2023-2025 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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

import scala.meta.{ Term, Type }

import Expression.Code
import PolyadicPi.*
import Calculus.*
import Encoding.*
import scala.util.parsing.combinator.pisc.parser.Expansion.Duplications


abstract class Calculus extends PolyadicPi:

  def equation(using Duplications): Parser[Bind] =
    invocation(true)<~"=" >> {
      case (bind, bound) =>
        _code = -1
        _dir = None
        given Bindings = Bindings() ++ bound.map(_ -> Occurrence(None, pos()))
        given Int = 1
        choice ^^ {
          case (_sum, _free) =>
            val sum = _sum.flatten
            val free = _free ++ sum.capitals
            if (free &~ bound).nonEmpty
            then
              throw EquationFreeNamesException(bind.identifier, free &~ bound)
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
          else if _scaling && emitter.canScale
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
          else if _scaling && emitter.canScale
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
    "!"~> scale ~ opt( pace ) ~ opt( "."~>μ<~"." ) >> { // [guarded] replication
      case _ ~ _ ~ Some((π(λ(ch: Symbol), Some(cons), _, _*), _)) if cons.nonEmpty && cons != "ν" =>
        throw ConsGuardParsingException(cons, ch.name)
      case parallelism ~ pace ~ Some(π @ (π(λ(ch: Symbol), Some(cons), _, params*), _)) =>
        var parallelismʹ = if parallelism < 0 then _replication._1 else parallelism
        parallelismʹ = if parallelismʹ < 2 || !emitter.featuresLinearReplication || !_replication._2 then parallelismʹ else -parallelismʹ
        if params.filter(_.isSymbol).exists(_.asSymbol == ch)
        then
          if emitter.hasReplicationInputGuardFlaw(parallelismʹ)
          then
            warn(throw GuardParsingException(ch.name, cons.isEmpty))
        val (bound, freeʹ) = π._2
        PendingOccurrence(freeʹ)
        BindingOccurrence(bound)
        choice ^^ {
          case (sum, free) =>
            `!`(parallelismʹ, pace, Some(π._1), sum) -> (freeʹ ++ (free &~ bound))
        }
      case parallelism ~ pace ~ Some(μ) =>
        var parallelismʹ = if parallelism < 0 then _replication._1 else parallelism
        parallelismʹ = if parallelismʹ < 2 || !emitter.featuresLinearReplication || !_replication._2 then parallelismʹ else -parallelismʹ
        val (_, freeʹ) = μ._2
        PendingOccurrence(freeʹ)
        choice ^^ {
          case (sum, free) =>
            `!`(parallelismʹ, pace, Some(μ._1), sum) -> (freeʹ ++ free)
        }
      case parallelism ~ pace ~ _ =>
        var parallelismʹ = if parallelism < 0 then _replication._1 else parallelism
        parallelismʹ = if parallelismʹ < 2 || !emitter.featuresLinearReplication || !_replication._2 then parallelismʹ else -parallelismʹ
        choice ^^ {
          case (sum, free) =>
            `!`(parallelismʹ, pace, None, sum) -> free
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
    qual ~ IDENT ~ opt( "("~> names ~ opt(if equation then "*" else "") <~")" ) ^^ {
      case qual ~ identifier ~ _ if equation && qual.nonEmpty =>
        throw EquationQualifiedException(identifier, qual)
      case _ ~ identifier ~ Some(params ~ _) if equation && !params.forall(_._1.isSymbol) =>
        throw EquationParamsException(identifier, params.filterNot(_._1.isSymbol).map(_._1)*)
      case qual ~ "Self" ~ Some(params ~ init) =>
        val paramsʹ = if equation && init.isDefined
                      then params.map(_._1).init
                      else params.map(_._1)
        self += _code
        `(*)`("Self_" + _code, qual, paramsʹ*) -> params.map(_._2).reduce(_ ++ _)
      case qual ~ "Self" ~ _ =>
        self += _code
        `(*)`("Self_" + _code, qual) -> Names()
      case qual ~ identifier ~ Some(params ~ init) =>
        val paramsʹ = if equation && init.isDefined
                      then params.map(_._1).init
                      else params.map(_._1)
        identifier match
          case s"Self_$n" if (try { n.toInt; true } catch _ => false) =>
            self += n.toInt
          case _ =>
        `(*)`(identifier, qual, paramsʹ*) -> params.map(_._2).reduce(_ ++ _)
      case qual ~ identifier ~ _ =>
        identifier match
          case s"Self_$n" if (try { n.toInt; true } catch _ => false) =>
            self += n.toInt
          case _ =>
        `(*)`(identifier, qual) -> Names()
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
    rep(qual_r) ^^ { _.map(_.stripPrefix("{").stripSuffix("}")) }


object Calculus:

  private val qual_r = "[{][^}]*[}]".r

  type Bind = (`(*)`, +)

  export Pre.*
  export AST.*

  enum Pre:

    case ν(names: String*) // forcibly

    case τ(code: Option[Code])

    case π(channel: λ, polarity: Option[String], code: Option[Code], names: λ*)

    override def toString: String = this match
      case ν(names*) => names.mkString("ν(", ", ", ")")
      case π(channel, polarity, _, names*) =>
        if polarity.isDefined
        then
          if polarity.get != "ν"
          then "" + channel + names.mkString(s"(${polarity.get}", ", ", ").")
          else "" + channel + names.mkString("<ν", ", ", ">.")
        else "" + channel + names.mkString("<", ", ", ">.")
      case _ => "τ."

  given `_π_`: {} with
    extension (self: π)
      def copy(channel: λ = self.channel,
               polarity: Option[String] = self.polarity,
               code: Option[Code] = self.code,
               names: Seq[λ] = self.names): π =
        π(channel, polarity, code, names*)

  enum AST:

    case +(scaling: Int, choices: AST.∥ *)

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
               qual: List[String],
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
          "if " + test + " " + t + " else " + f.get

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

      case `(*)`(identifier, qual, params*) =>
        import emitter.shared.Meta.\
        val args = params.map(_.toTerm).toList
        val term = qual match
          case h :: t => (t.map(\(_)) :+ \("π") :+ \(identifier)).foldLeft(h: Term)(Term.Select(_, _))
          case _ => \(identifier)
        Term.Apply(term, Term.ArgClause(args)).toString

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
               qual: List[String] = self.qual,
               params: Seq[λ] = self.params): `(*)` =
        `(*)`(identifier, qual, params*)

  object ∅ :
    def apply(): + = `+`(-1)
    def unapply(self: AST): Boolean = self match
      case sum: + => sum.isVoid
      case _ => false

  case class λ(`val`: Any)(using val `type`: Option[(Type, Option[Type])] = None):
    val isSymbol: Boolean = `val`.isInstanceOf[Symbol]
    def asSymbol: Symbol = `val`.asInstanceOf[Symbol]

    val kind: String = `val` match
      case _: Symbol => "channel name"
      case _: BigDecimal => "decimal number"
      case _: Boolean => "True False"
      case _: String => "string literal"
      case _: Term => "Scalameta Term"

    def toTerm: Term =
      import scala.meta._
      import dialects.Scala3
      `val` match
        case it: Symbol => Term.Name(it.name)
        case it: BigDecimal => Term.Apply(Term.Name("BigDecimal"), Term.ArgClause(Lit.String(it.toString)::Nil))
        case it: Boolean => Lit.Boolean(it)
        case it: String => Lit.String(it)
        case it: Term => it

    override def toString: String = `val` match
      case it: Symbol => it.name
      case it: BigDecimal => "" + it
      case it: Boolean => it.toString.capitalize
      case it: String => "\"" + it + "\""
      case it: Term => "/*" + it + "*/"


  // exceptions

  import Expression.ParsingException

  abstract class EquationParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class EquationQualifiedException(identifier: String, qual: List[String])
      extends EquationParsingException(s"""A qualified package ${qual.mkString(".")} is present in the left hand side of $identifier""")

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

  case class GuardParallelismNot1ParsingException(emitter: Emitter, parallelism: Int, name: String)
      extends PrefixParsingException(s"""Emitter `$emitter' assigns parallelism 1 (≠ $parallelism) to a replication guard with channel name "$name"""")


  // functions

  extension (sum: +)
    private def isVoid: Boolean = sum match
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
