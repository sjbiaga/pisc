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

package masc
package parser

import scala.collection.mutable.{ LinkedHashSet => Set }

import scala.meta.Term

import Expression.Code
import Ambient.*
import Calculus.*
import Encoding.*


abstract class Calculus extends Ambient:

  def equation: Parser[Bind] =
    invocation(true)<~"=" >> {
      case (bind, bound) =>
        _code = -1
        given Bindings = Bindings() ++ bound.map(_ -> Occurrence(None, pos()))
        parallel ^^ {
          case (_par, _free) =>
            val par = _par.flatten
            val free = _free ++ par.capitals
            if (free &~ bound).nonEmpty
            then
              throw EquationFreeNamesException(bind.identifier, free &~ bound)
            bind -> par
        }
    }

  def parallel(using Bindings): Parser[(∥, Names)] =
    rep1sep(sequential, "|") ^^ { ss =>
      ∥(ss.map(_._1)*) -> ss.map(_._2).reduce(_ ++ _)
    }

  def sequential(using bindings: Bindings): Parser[(`.`, Names)] =
    given Bindings = Bindings(bindings)
    prefixes ~ opt( leaf | "("~>parallel<~")" ) ^^ { `pre ~ opt` =>
      bindings ++= binders
      `pre ~ opt` match
        case (ps, (bound, free)) ~ Some((end, free2)) =>
          `.`(end, ps*) -> (free ++ (free2 &~ bound))
        case (ps, (_, free)) ~ _ =>
          `.`(∥(), ps*) -> free // void
    }

  def leaf(using Bindings): Parser[(-, Names)] =
    "!" ~> opt( "."~> "("~>name<~")" <~"." ) ~ parallel ^^ { // [guarded] replication
      case Some((it, bound)) ~ (par, free) =>
        `!`(Some(it), par) -> (free &~ bound)
      case None ~ (par, free) =>
        `!`(None, par) -> free
    } |
    name ~ ("["~>parallel<~"]") ^^ { // ambient
      case (amb, name) ~ (par, free) =>
        `[]`(amb, par) -> (name ++ free)
    } |
    ("<"~>caps<~">") ~ opt( expression ) ^^ { // output action
      case _ ~ Some(((Left(enums), _), _)) =>
        throw TermParsingException(enums)
      case (path, free) ~ Some((it @ (Right(_), _), free2)) =>
        <>(Some(it), path*) -> (free ++ free2)
      case (path, free) ~ _ =>
        <>(None, path*) -> free
    } |
    "go" ~> name ~ ("."~> parallel) ^^ { // objective move
      case (amb, name) ~ (par, free) =>
        `go.`(amb, par) -> (name ++ free)
    } |
    capital |
    invocation() |
    instantiation

  def instantiation(using Bindings): Parser[(`⟦⟧`, Names)]

  def capital: Parser[(`{}`, Names)]

  def prefixes(using Bindings): Parser[(List[Pre], (Names, Names))] =
    rep(prefix) ^^ { ps =>
      val bound = ps.map(_._2._1)
      val free = ps.map(_._2._2)
        .zipWithIndex
        .foldLeft(Names()) { case (r, (ns, i)) =>
          val bs = bound.take(i)
          r ++= ns.filter { n => bs.indexWhere(_.contains(n)) < 0 }
        }
      ps.map(_._1) -> (if bound.nonEmpty then bound.reduce(_ ++ _) else Names(), free)
    }

  def prefix(using Bindings): Parser[(Pre, (Names, Names))] =
    "ν"~>"("~>rep1sep(name, ",")<~")" ^^ { ns => // restriction
      val bound = ns.map(_._2).reduce(_ ++ _)
      BindingOccurrence(bound)
      ν(ns.map(_._1)*) -> (bound, Names())
    } |
    "τ" ~> opt( expression ) <~ "." ^^ { // silent transition
      case Some((it, free)) =>
        τ(Some(it)) -> (Names(), free)
      case _ =>
        τ(None) -> (Names(), Names())
    } |
    caps <~ "." ^^ { // capability action
      case (path, free) =>
        `,.`(path*) -> (Names(), free)
    } |
    ("("~>name<~")") ~ opt( expression ) <~ "." ^^ { case (name, bound) ~ opt => // input action
      BindingOccurrence(bound)
      opt match
        case Some(((Left(enums), _), _)) =>
          throw TermParsingException(enums)
        case Some((it @ (Right(_), _), free)) =>
          `()`(name, Some(it)) -> (bound, free)
        case _ =>
          `()`(name, None) -> (bound, Names())
    }

  def invocation(equation: Boolean = false): Parser[(`(*)`, Names)] =
    qual ~ IDENT ~ opt( "("~>rep1sep(name, ",")<~")" ) ^^ {
      case qual ~ id ~ _ if equation && qual.nonEmpty =>
        throw EquationQualifiedException(id, qual)
      case qual ~ "Self" ~ Some(params) =>
        self += _code
        `(*)`("Self_" + _code, qual, params.map(_._1)*) -> params.map(_._2).reduce(_ ++ _)
      case qual ~ "Self" ~ _ =>
        self += _code
        `(*)`("Self_" + _code, qual) -> Names()
      case qual ~ id ~ Some(params) =>
        id match
          case s"Self_$n" if (try { n.toInt; true } catch _ => false) =>
            self += n.toInt
          case _ =>
        `(*)`(id, qual, params.map(_._1)*) -> params.map(_._2).reduce(_ ++ _)
      case qual ~ id ~ _ =>
        id match
          case s"Self_$n" if (try { n.toInt; true } catch _ => false) =>
            self += n.toInt
          case _ =>
        `(*)`(id, qual) -> Names()
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

  type Bind = (`(*)`, ∥)

  export Pre.*
  export AST.*

  enum Pre:

    case ν(names: String*) // forcibly

    case τ(code: Option[Code])

    case `,.`(path: Ambient.AST*)

    case `()`(name: String, code: Option[Code])

    override def toString: String = this match
      case ν(names*) => names.mkString("ν(", ", ", ")")
      case `,.`(path*) => path.mkString("", ", ", ".")
      case `()`(name, _) => s"($name)."
      case _ => "τ."

  enum AST:

    case ∥(components: AST.`.`*)

    case `.`(end: &, prefixes: Pre*)

    case <>(code: Option[Code], path: Ambient.AST*)

    case !(guard: Option[String], par: AST.∥)

    case `[]`(amb: String, par: AST.∥)

    case `go.`(amb: String, par: AST.∥)

    case `⟦⟧`(definition: Definition,
              variables: Names,
              par: AST.∥,
              assign: Option[Set[(String, String)]] = None)

    case `{}`(identifier: String,
              pointers: List[String],
              agent: Boolean = false,
              params: String*)

    case `(*)`(identifier: String,
               qual: List[String],
               params: String*)

    override def toString: String = this match
      case ∅(_) => "()"
      case ∥(components*) => components.mkString(" | ")

      case `.`(∅(_)) => "()"
      case `.`(∅(_), prefixes*) => prefixes.mkString(" ") + " ()"
      case `.`(end: ∥, prefixes*) =>
        prefixes.mkString(" ") + (if prefixes.isEmpty then "" else " ") + "(" + end + ")"
      case `.`(end, prefixes*) =>
        prefixes.mkString(" ") + (if prefixes.isEmpty then "" else " ") + end

      case <>(_, path*) => path.mkString("<", ", ", ">")

      case !(guard, par) => "!" + guard.map(".(" + _ + ").").getOrElse("") + par

      case `[]`(amb, ∅(_)) => amb + " [ ]"
      case `[]`(amb, par) => amb + " [ " + par + " ]"

      case `go.`(amb, par) => "go " + amb + "." + par

      case `⟦⟧`(definition, variables, par, assign) =>
        val vars = ( if variables.nonEmpty
                     then
                       variables.map {
                         case it if assign.map(_.exists(_._1 == it)).getOrElse(false) =>
                           s"$it = ${assign.get.find(_._1 == it).get._2}"
                         case it => it
                       }.mkString("{", ", ", "}")
                     else
                       ""
                   )
        s"""${Definition(definition.code, definition.term)}$vars = $par"""

      case `{}`(identifier, pointers, agent, params*) =>
        val ps = if agent then params.mkString("(", ", ", ")") else ""
        s"""$identifier$ps{${pointers.mkString(", ")}}"""

      case `(*)`(identifier, _) => identifier
      case `(*)`(identifier, _, params*) => s"$identifier(${params.mkString(", ")})"

  object ∅ :
    def unapply[T <: AST](self: T): Option[Unit] = self match
      case par: ∥ if par.isVoid => Some(())
      case _ => None

  private val qual_r = "[{][^}]*[}]".r


  // exceptions

  import Expression.ParsingException

  abstract class EquationParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class EquationQualifiedException(id: String, qual: List[String])
      extends EquationParsingException(s"""A qualified package ${qual.mkString(".")} is present in the left hand side of $id""")

  case class EquationFreeNamesException(id: String, free: Names)
      extends EquationParsingException(s"""The free names (${free.mkString(", ")}) in the right hand side are not formal parameters of the left hand side of $id""")

  import scala.meta.Enumerator

  case class TermParsingException(enums: List[Enumerator])
      extends ParsingException(s"The embedded Scalameta should be a Term, not Enumerator `$enums'")


  // functions

  extension (par: ∥)
    @annotation.tailrec
    private def isVoid: Boolean = par match
      case ∥() => true
      case ∥(`.`(par: ∥)) => par.isVoid
      case _ => false

  extension [T <: AST](ast: T)

    def flatten: T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅(_) => ast

        case ∥(`.`(par: ∥), it*) =>
          val lhs = par.flatten
          val rhs = ∥(it*).flatten
          ∥((lhs.components ++ rhs.components).filterNot(∥(_).isVoid)*)

        case ∥(seq, it*) =>
          val lhs: ∥ = ∥(seq.flatten)
          val rhs = ∥(it*).flatten
          ∥((lhs.components ++ rhs.components).filterNot(∥(_).isVoid)*)

        case `.`(∥(`.`(end, ps*)), it*) =>
          `.`(end, (it ++ ps)*).flatten

        case `.`(end, it*) =>
          `.`(end.flatten, it*)

        case !(None, par) =>
          par.flatten match
            case ∥(`.`(end: !)) => end
            case it => `!`(None, it)

        case it @ !(_, par) =>
          it.copy(par = par.flatten)

        case `[]`(amb, par) =>
          `[]`(amb, par.flatten)

        case `go.`(amb, par) =>
          `go.`(amb, par.flatten)

        case _ => ast
