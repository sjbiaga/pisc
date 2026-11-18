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

package masc
package parser

import scala.meta.Term

import Expression.Code
import Ambient.*
import Calculus.*
import Encoding.*
import scala.util.parsing.combinator.masc.parser.Expansion.Duplications


abstract class Calculus extends Ambient:

  def equation(using Duplications): Parser[Bind] =
    invocation(true) >> {
      case bind if _settings.exclude =>
        ".*".r ^^ { _ => bind -> ∅() }
      case bind =>
        val bound = bind.free
        _code = -1
        _directive = None
        given Bindings = Bindings() ++ bound.map(_ -> Occurrence(None, pos()))
        given Int = 1
        "="~> positioned(parallel) ^^ { _par =>
          val par = _par.flatten
          val free = _par.free ++ par.capitals
          if (free &~ bound).nonEmpty
          then
            throw EquationFreeNamesException(bind.identifier, free &~ bound)
          bind -> par
        }
    }

  def parallelʹ(using Bindings, Duplications, Int): Parser[∥] =
    opt( "("~>positioned(parallel)<~")" ) ^^ (_.getOrElse(∅()))

  def parallel(using Bindings, Duplications, Int): Parser[∥] =
    scale >> { scaling =>
      val scalingʹ = scaling.abs
      given Int = if scalingʹ == 1 then summon[Int] else scalingʹ
      rep1sep(positioned(sequential), "|") ^^ { it =>
        val ns = it.map(_.free)
        if scalingʹ == 0
        then
          ∅()
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
    prefixes ~ ( positioned(leaf) | positioned(parallelʹ) ) ^^ {
      case (it, (bound, free)) ~ (end: (∥ | -)) =>
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
    "ν"~>"("~>names<~")" ^^ { _.unzip match // restriction
      case (ns, bs) =>
        val bound = bs.reduce(_ ++ _)
        BindingOccurrence(bound)
        ν(ns*).bound = bound
    } |
    "τ" ~> opt( expression ) <~ "." ^^ { // silent transition
    case Some((it, free)) =>
      PendingOccurrence(free)
      τ(Some(it)).free = free
    case _ =>
      τ(None)
    } |
    caps <~ "." ^^ { // capability action
      case (path, free) =>
        PendingOccurrence(free)
        `..`(path*).free = free
    } |
    ("("~>name<~")") ~ opt( expression ) <~ "." ^^ {
      case _ ~ Some(((Left(enums), _), _)) =>
        throw TermParsingException(enums)
      case (name, bound) ~ Some((it, free)) =>
        PendingOccurrence(free)
        BindingOccurrence(bound)
        (`()`(name, Some(it)).bound = bound).free = free
      case (name, bound) ~ _ =>
        BindingOccurrence(bound)
        `()`(name, None).bound = bound
    }

  def leaf(using Bindings, Duplications, Int): Parser[AST] =
    "!"~> scale ~ opt( pace ) ~ opt( "."~> "("~>name<~")" <~"." ) >> { // [guarded] replication
      case parallelism ~ pace ~ Some((it, bound)) =>
        BindingOccurrence(bound)
        positioned(parallel) ^^ { par =>
          `!`(parallelism, pace, Some(it), par).free = par.free &~ bound
        }
      case parallelism ~ pace ~ _ =>
        positioned(parallel) ^^ { par =>
          `!`(parallelism, pace, None, par).free = par.free
        }
    } |
    name >> { // ambient
      case (amb, name) =>
        PendingOccurrence(name)
        "["~> positioned(parallel) <~"]" ^^ { par =>
          `[]`(amb, par).free = name ++ par.free
        }
    } |
    ("<"~> opt( caps ) <~">") ~ opt( expression ) ^^ { // output action
      case _ ~ Some(((Left(enums), _), _)) =>
        throw TermParsingException(enums)
      case Some((path, free)) ~ Some((it @ (Right(_), _), freeʹ)) =>
        PendingOccurrence(free ++ freeʹ)
        <>(Some(it), path*).free = free ++ freeʹ
      case Some((path, free)) ~ _ =>
        PendingOccurrence(free)
        <>(None, path*).free = free
      case _ =>
        <>(None)
    } |
    "go"~> name <~"."  >> { // objective move
      case (amb, name) =>
        PendingOccurrence(name)
        positioned(parallel) ^^ { par =>
          `go.`(amb, par).free = name ++ par.free
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

  def invocation(equation: Boolean = false): Parser[`(*)`] =
    qual ~ IDENT ~ opt( "("~> names ~ opt(if equation then "*" else "") <~")" ) ^^ {
      case qual ~ identifier ~ _ if equation && qual.nonEmpty =>
        throw EquationQualifiedException(identifier, qual)
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

  export ast.{ AST, Bind, -, Cap, Pre }
  export Cap.*, Pre.*, AST.*


  // exceptions

  import Expression.ParsingException

  abstract class EquationParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class EquationQualifiedException(identifier: String, qual: List[String])
      extends EquationParsingException(s"""A qualified package ${qual.mkString(".")} is present in the left hand side of $identifier""")

  case class EquationFreeNamesException(identifier: String, free: Names)
      extends EquationParsingException(s"""The free names (${free.mkString(", ")}) in the right hand side are not formal parameters of the left hand side of $identifier""")

  import scala.meta.Enumerator

  case class TermParsingException(enums: List[Enumerator])
      extends ParsingException(s"The embedded Scalameta should be a Term, not Enumerator `$enums'")


  // functions

  extension [T <: AST](ast: T)

    def flatten: T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      given (Int => (Int, Int)) = ast.lc

      ast match

        case ∅() =>
          ∅(ast.pos)

        case it @ ∥(-1|1, `.`(par @ ∥(-1|1, _*)), components*) =>
          val lhs = par.flatten
          val rhs = ∥(-1, components*).flatten
          it.cc(components = lhs.components.filterNot(_.isVoid) ++ rhs.components)

        case it @ ∥(_, seq, components*) =>
          val lhs: ∥ = ∥(-1, seq.flatten)
          val rhs = ∥(-1, components*).flatten
          it.cc(components = lhs.components.filterNot(_.isVoid) ++ rhs.components)

        case it @ `.`(∥(-1|1, `.`(end, psr*)), psl*) =>
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

        case it @ !(-1, None, None, par) =>
          val itʹ = par.flatten match
            case ∥(-1|1, `.`(end: !)) => end
            case parʹ => it.cc(par = parʹ)
          if itʹ.isVoid
          then
            ∅(ast.pos)
          else
            itʹ

        case it @ !(_, _, _, par) =>
          val itʹ = it.cc(par = par.flatten)
          if itʹ.isVoid
          then
            ∅(ast.pos)
          else
            itʹ

        case it @ `[]`(_, par) =>
          it.cc(par = par.flatten)

        case it @ `go.`(_, par) =>
          it.cc(par = par.flatten)

        case it @ `⟦⟧`(definition @ Definition(_, _, _, variables, _), par, _, pointers)
            if variables.size > pointers.size =>
          val names = variables.drop(pointers.size)
          val (pos, free) = (par.pos, par.free -- names)
          val νʹ: ν = ν(names.toSeq*).setPos(pos).bound = names
          val seq = `.`(par, νʹ).setPos(pos).free = free
          val parʹ = ∥(-1, seq).setPos(pos).free = free
          val itʹ = it.cc(definition = definition.copy(variables = variables.take(pointers.size)), par = parʹ)
          if itʹ.isVoid
          then
            ∅(ast.pos)
          else
            itʹ

        case it: `⟦⟧` if it.isVoid =>
          ∅(ast.pos)

        case _ => ast
