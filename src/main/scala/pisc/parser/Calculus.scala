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

import PolyadicPi.*
import Calculus.*
import Encoding.*
import scala.util.parsing.combinator.pisc.parser.Expansion.Duplications


abstract class Calculus extends PolyadicPi:

  def equation(using Duplications): Parser[Bind] =
    positioned(invocation(true)) >> {
      case bind if _settings.exclude =>
        ".*".r ^^ { _ => bind -> ∅() }
      case bind =>
        val bound = bind.free
        _code = -1
        _directive = None
        given Bindings = Bindings() ++ bound.map(_ -> Occurrence(None, pos()))
        given Int = 1
        "="~> positioned(choice) ^^ { _sum =>
          val sum = _sum.flatten
          val free = _sum.free ++ sum.capitals
          if (free &~ bound).nonEmpty
          then
            throw EquationFreeNamesException(bind.identifier, free &~ bound)
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
        else if scalingʹ == 1
        then
          `+`(scaling, it*).free = ns.reduce(_ ++ _)
        else if _settings.scaling && emitter.canScale
        then
          `+`(scalingʹ, it*).free = ns.reduce(_ ++ _)
        else
          `+`(-1, List.fill(scalingʹ)(it).flatten*).free = ns.reduce(_ ++ _)
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
        else if scalingʹ == 1
        then
          ∥(scaling, it*).free = ns.reduce(_ ++ _)
        else if _settings.scaling && emitter.canScale
        then
          ∥(scalingʹ, it*).free = ns.reduce(_ ++ _)
        else
          ∥(-1, List.fill(scalingʹ)(it).flatten*).free = ns.reduce(_ ++ _)
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
    positioned(μ(None))<~"." ^^ { it =>
      PendingOccurrence(it.free)
      BindingOccurrence(it.bound)
      it
    }

  def leaf(using Bindings, Duplications, Int): Parser[AST] =
    "["~condition~"]"~positioned(choice) ^^ { // (mis)match
      case _ ~ cond ~ _ ~ t =>
        ?:(cond._1, t, None).free = cond._2 ++ t.free
    } |
    "if"~condition~"then"~positioned(choice)~"else"~positioned(choice) ^^ { // if then else
      case _ ~ cond ~ _ ~ t ~ _ ~ f =>
        ?:(cond._1, t, Some(f)).free = cond._2 ++ (t.free ++ f.free)
    } |
    condition~"?"~positioned(choice)~":"~positioned(choice) ^^ { // Elvis operator
      case cond ~ _ ~ t ~ _ ~ f =>
        ?:(cond._1, t, Some(f)).free = cond._2 ++ (t.free ++ f.free)
    } |
    ("!"|"¡") ~ scale >> { // [guarded] replication
      case lin ~ parallelism =>
        var parallelismʹ = if parallelism == -1 then _settings.replication._1 else parallelism
        if parallelismʹ == -1 && lin == "¡" then parallelismʹ = 1
        if parallelismʹ.abs == 1 && (_settings.replication._2 || lin == "¡") && emitter.featuresModuloReplication then parallelismʹ = Int.MinValue
        parallelismʹ = if parallelismʹ < 2 || !(_settings.replication._2 || lin == "¡") || !emitter.featuresModuloReplication then parallelismʹ else -parallelismʹ
        opt( pace ) ~ opt( "."~>positioned(μ(Some(parallelismʹ)))<~"." ) >> {
          case _ ~ Some(π(λ(ch: Symbol), Some(cons), _, _*)) if cons.nonEmpty && cons != "ν" =>
            throw ConsGuardParsingException(cons, ch.name)
          case pace ~ Some(π @ π(λ(ch: Symbol), Some(cons), _, params*)) =>
            if params.filter(_.isSymbol).exists(_.asSymbol == ch)
            then
              if emitter.hasReplicationInputGuardFlaw(parallelismʹ)
              then
                warn(throw GuardParsingException(ch.name, cons.isEmpty))
            val (bound, free) = π.bound -> π.free
            PendingOccurrence(free)
            BindingOccurrence(bound)
            positioned(choice) ^^ { sum =>
              `!`(parallelismʹ, pace, Some(π), sum).free = free ++ (sum.free &~ bound)
            }
          case pace ~ Some(μ) =>
            val free = μ.free
            PendingOccurrence(free)
            positioned(choice) ^^ { sum =>
              `!`(parallelismʹ, pace, Some(μ), sum).free = free ++ sum.free
            }
          case pace ~ _ =>
            positioned(choice) ^^ { sum =>
              `!`(parallelismʹ, pace, None, sum).free = sum.free
            }
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

  def instantiation(using Bindings, Duplications, Int): Parser[`⟦⟧`]

  def condition(using Bindings): Parser[(((λ, λ), Boolean), Names)] =
    name~("="|"≠")~name ^^ {
      case (lhs, free_lhs) ~ mismatch ~ (rhs, free_rhs) =>
        val free = free_lhs ++ free_rhs
        PendingOccurrence(free)
        (lhs -> rhs -> (mismatch != "=")) -> free
    }

  def invocation(equation: Boolean = false): Parser[`(*)`] =
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
        `(*)`("Self_" + _code, qual, paramsʹ*).free = params.map(_._2).reduce(_ ++ _)
      case qual ~ "Self" ~ _ =>
        self += _code
        `(*)`("Self_" + _code, qual)
      case qual ~ identifier ~ Some(params ~ init) =>
        val paramsʹ = if equation && init.isDefined
                      then params.map(_._1).init
                      else params.map(_._1)
        identifier match
          case s"Self_$n" if (try { n.toInt; true } catch _ => false) =>
            self += n.toInt
          case _ =>
        `(*)`(identifier, qual, paramsʹ*).free = params.map(_._2).reduce(_ ++ _)
      case qual ~ identifier ~ _ =>
        identifier match
          case s"Self_$n" if (try { n.toInt; true } catch _ => false) =>
            self += n.toInt
          case _ =>
        `(*)`(identifier, qual)
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

  export ast.{ AST, Bind, Cast, λ, -, μ, Pre }
  export Pre.*, AST.*


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


  // functions

  extension [T <: AST](ast: T)

    def flatten: T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      given (Int => (Int, Int)) = ast.lc

      ast match

        case ∅() =>
          ∅(ast.pos)

        case it @ +(-1|1, ∥(-1|1, `.`(sum @ +(-1|1, _*))), choices*) =>
          val lhs = sum.flatten
          val rhs = `+`(-1, choices*).flatten
          it.cc(choices = lhs.choices.filterNot(`+`(-1, _).isVoid) ++ rhs.choices) match
            case ∅() => ∅(ast.pos)
            case itʹ => itʹ

        case it @ +(_, par, choices*) =>
          val lhs = `+`(-1, par.flatten)
          val rhs = `+`(-1, choices*).flatten
          it.cc(choices = lhs.choices.filterNot(`+`(-1, _).isVoid) ++ rhs.choices) match
            case ∅() => ∅(ast.pos)
            case itʹ => itʹ

        case it @ ∥(-1|1, `.`(+(-1|1, par @ ∥(-1|1, _*))), components*) =>
          val lhs = par.flatten
          val rhs = ∥(-1, components*).flatten
          it.cc(components = lhs.components.filterNot(_.isVoid) ++ rhs.components)

        case it @ ∥(_, seq, components*) =>
          val lhs: ∥ = ∥(-1, seq.flatten)
          val rhs = ∥(-1, components*).flatten
          it.cc(components = lhs.components.filterNot(_.isVoid) ++ rhs.components)

        case it @ `.`(+(-1|1, ∥(-1|1, `.`(end, psr*))), psl*) =>
          val itʹ = it.cc(end = end, prefixes = psl ++ psr).flatten
          if itʹ.isVoid
          then
            `.`(∅(ast.pos)).setPos(ast.pos)
          else
            itʹ

        case it @ `.`(end, _*) =>
          val itʹ = it.cc(end = end.flatten)
          if itʹ.isVoid
          then
            `.`(∅(ast.pos)).setPos(ast.pos)
          else
            itʹ

        case it @ ?:(_, t, f) =>
          val itʹ = it.cc(t = t.flatten, f = f.map(_.flatten))
          if itʹ.isVoid
          then
            ∅(ast.pos)
          else
            itʹ

        case it @ !(-1, None, None, sum) =>
          val itʹ = sum.flatten match
            case +(-1|1, ∥(-1|1, `.`(end: !))) => end
            case sumʹ => it.cc(sum = sumʹ)
          if itʹ.isVoid
          then
            ∅(ast.pos)
          else
            itʹ

        case it @ !(_, _, _, sum) =>
          val itʹ = it.cc(sum = sum.flatten)
          if itʹ.isVoid
          then
            ∅(ast.pos)
          else
            itʹ

        case it @ `⟦⟧`(definition @ Definition(_, _, _, variables, _), sum, _, pointers)
            if variables.size > pointers.size =>
          val names = variables.drop(pointers.size)
          val (pos, free) = (sum.pos, sum.free -- names)
          val νʹ: ν = ν(names.map(_.name).toSeq*).setPos(pos).bound = names
          val seq = `.`(sum, νʹ).setPos(pos).free = free
          val par = ∥(-1, seq).setPos(pos).free = free
          val sumʹ = `+`(-1, par).setPos(pos).free = free
          val itʹ = it.cc(definition = definition.copy(variables = variables.take(pointers.size)), sum = sumʹ)
          if itʹ.isVoid
          then
            ∅(ast.pos)
          else
            itʹ

        case it: `⟦⟧` if it.isVoid =>
          ∅(ast.pos)

        case _ => ast
