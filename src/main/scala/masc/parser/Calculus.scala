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

package masc
package parser

import scala.collection.mutable.{ LinkedHashSet => Set }

import scala.meta.Term

import scala.util.parsing.combinator._

import Expression.Code
import Ambient._
import Calculus._
import Encoding._


abstract class Calculus extends Ambient:

  def equation: Parser[Bind] =
    invocation(true)<~"=" >> {
      case (bind, binding) =>
        _code = -1
        given Names2 = Names2() ++ binding.map(_ -> Occurrence(None, pos()))
        parallel ^^ {
          case (_par, _free) =>
            val par = _par.flatten
            val free = _free ++ par.capitals
            if (free &~ binding).nonEmpty
            then
              throw EquationFreeNamesException(bind.identifier, free &~ binding)
            bind -> par
        }
    }

  def parallel(using binding2: Names2): Parser[(||, Names)] =
    rep1sep(sequential, "|") ^^ { ss =>
      ||(ss.map(_._1)*) -> ss.map(_._2).reduce(_ ++ _)
    }

  def sequential(using binding2: Names2): Parser[(`.`, Names)] =
    given Names2 = Names2(binding2)
    prefixes ~ opt( leaf | "("~>parallel<~")" ) ^^ { `pre ~ opt` =>
      binding2 ++= binders
      `pre ~ opt` match
        case pre ~ Some((end, free)) =>
          `.`(end, pre._1*) -> (pre._2._2 ++ (free &~ pre._2._1))
        case pre ~ _ =>
          `.`(∅, pre._1*) -> pre._2._2 // void
    }

  def leaf(using binding2: Names2): Parser[(-, Names)] =
    "!" ~> opt( "."~> "("~>name<~")" <~"." ) ~ parallel ^^ { // [guarded] replication
      case Some((it, binding)) ~ (par, free) =>
        `!`(Some(it), par) -> (free &~ binding)
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

  def instantiation(using Names2): Parser[(`⟦⟧`, Names)]

  def capital: Parser[(`{}`, Names)]

  def prefixes(using binding2: Names2): Parser[(List[Pre], (Names, Names))] =
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

  def prefix(using binding2: Names2): Parser[(Pre, (Names, Names))] =
    "ν"~>"("~>rep1sep(name, ",")<~")" ^^ { ns => // restriction
      val binding = ns.map(_._2).reduce(_ ++ _)
      Names2(binding)
      ν(ns.map(_._1)*) -> (binding, Names())
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
    ("("~>name<~")") ~ opt( expression ) <~ "." ^^ { case (name, binding) ~ opt => // input action
      Names2(binding)
      opt match
        case Some(((Left(enums), _), _)) =>
          throw TermParsingException(enums)
        case Some((it @ (Right(_), _), free)) =>
          `()`(name, Some(it)) -> (binding, free)
        case _ =>
          `()`(name, None) -> (binding, Names())
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
    rep("""[{][^}]*[}]""".r) ^^ { _.map(_.stripPrefix("{").stripSuffix("}")) }


object Calculus:

  import scala.annotation.tailrec

  type Bind = (`(*)`, ||)

  sealed trait AST extends Any

  case class ||(components: `.`*) extends AST:
    override def toString: String = components.mkString(" | ")

  object ∅ extends ||():
    override def canEqual(that: Any): Boolean =
      that.isInstanceOf[||]

    override def equals(any: Any): Boolean = any match
      case that: || => that.components.size == 0
      case _ => false

    override def toString: String = "()"

  case class `.`(end: &, prefixes: Pre*) extends AST:
    override def toString: String =
      prefixes.mkString(" ") + (if prefixes.isEmpty then "" else " ") + (if ∅ != end && end.isInstanceOf[||]
                                                                         then "(" + end + ")" else end)

  sealed trait Pre extends Any

  case class ν(names: String*) extends AnyVal with Pre: // forcibly
    override def toString: String = names.mkString("ν(", ", ", ")")

  case class τ(code: Option[Code]) extends AnyVal with Pre:
    override def toString: String = "τ."

  case class `,.`(path: Ambient.AST*) extends Pre:
    override def toString: String = path.mkString("", ", ", ".")

  case class `()`(name: String, code: Option[Code]) extends Pre:
    override def toString: String = s"($name)."

  case class <>(code: Option[Code], path: Ambient.AST*) extends AST:
    override def toString: String = path.mkString("<", ", ", ">")

  case class !(guard: Option[String], par: ||) extends AST:
    override def toString: String = "!" + guard.map("." + _).getOrElse("") + par

  case class `[]`(amb: String, par: ||) extends AST:
    override def toString: String = amb + (if ∅ == par then " [ ]" else " [ " + par + " ]")

  case class `go.`(amb: String, par: ||) extends AST:
    override def toString: String = "go " + amb + "." + par

  case class `⟦⟧`(definition: Definition,
                  variables: Names,
                  par: ||,
                  assign: Option[Set[(String, String)]] = None) extends AST:
    override def toString: String =
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

  case class `{}`(identifier: String,
                  pointers: List[String],
                  agent: Boolean = false,
                  params: String*) extends AST:
    override def toString: String =
      val ps = if agent then params.mkString("(", ", ", ")") else ""
      s"""$identifier$ps{${pointers.mkString(", ")}}"""

  case class `(*)`(identifier: String,
                   qual: List[String],
                   params: String*) extends AST:
    override def toString: String =
      if params.isEmpty
      then identifier
      else s"$identifier(${params.mkString(", ")})"


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

  extension[T <: AST](ast: T)

    def flatten: T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅ => ∅

        case ||(`.`(par: ||), it*) =>
          val lhs = par.flatten
          val rhs = ||(it*).flatten
          ||((lhs.components ++ rhs.components).filterNot(∅ == ||(_))*)

        case ||(seq, it*) =>
          val lhs = ||(seq.flatten)
          val rhs = ||(it*).flatten
          ||((lhs.components ++ rhs.components).filterNot(∅ == ||(_))*)

        case `.`(||(`.`(end, ps*)), it*) =>
          `.`(end, (it ++ ps)*).flatten

        case `.`(end, it*) =>
          `.`(end.flatten, it*)

        case !(None, par) =>
          par.flatten match
            case ||(`.`(end: !)) => end
            case it => `!`(None, it)

        case it @ !(_, par) =>
          it.copy(par = par.flatten)

        case `[]`(amb, par) =>
          `[]`(amb, par.flatten)

        case `go.`(amb, par) =>
          `go.`(amb, par.flatten)

        case _ => ast
