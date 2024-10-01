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

import scala.collection.mutable.{ ListBuffer => MutableList, LinkedHashSet => Set }

import scala.meta.{ Enumerator, Term }

import scala.util.parsing.combinator._

import Ambient._
import Calculus._

import scala.util.parsing.combinator.masc.parser.Extension.rename


class Calculus extends Ambient:

  def line: Parser[Either[Bind, Define]] =
    equation ^^ { Left(_) } | definition ^^ { Right(_) }

  def equation: Parser[Bind] =
    agent(true)~"="~parallel ^^ {
      case (bind, binding) ~ _ ~ (par, free)
        if (free &~ binding).nonEmpty =>
        throw EquationFreeNamesException(bind.identifier, free &~ binding)
      case (bind, _) ~ _ ~ (par, _) =>
        bind -> par.flatten
    }

  def definition: Parser[Define] =
    encoding ~ opt( "("~>rep1sep(name, ",")<~")" ) ~ opt( "{"~>rep1sep(name, ",")<~"}" ) ~"="~ parallel ^^ {
      case (term, binding1) ~ binding2 ~ _bound ~ _ ~ (_par, _free) =>
        val par = _par.flatten
        val free = (_free ++ par.capitals)
          .filterNot { it =>
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
        if !binding1.exists(_.charAt(0).isUpper)
        then
          eqtn :+= `(*)`("Self_" + code, Nil, (binding1 ++ const ++ bound).toSeq*) -> par
        Encoding(code, term, const, bound) -> par
    }

  def parallel: Parser[(`|`, Names)] =
    rep1sep(sequential, "|") ^^ { ss =>
      `|`(ss.map(_._1)*) -> ss.map(_._2).reduce(_ ++ _)
    }

  def sequential: Parser[(`.`, Names)] =
    prefixes ~ opt( leaf | "("~>parallel<~")" ) ^^ {
      case pre ~ Some((end, free)) =>
        `.`(end, pre._1*) -> (pre._2._2 ++ (free &~ pre._2._1))
      case pre ~ _ =>
        `.`(∅, pre._1*) -> pre._2._2 // void
    }

  def leaf: Parser[(`-`, Names)] =
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
      case _ ~ Some((Left(enums), _)) =>
        throw TermParsingException(enums)
      case (path, free) ~ Some((Right(it), free2)) =>
        `<>`(Some(it), path*) -> (free ++ free2)
      case (path, free) ~ _ =>
        `<>`(None, path*) -> free
    } |
    "go" ~> name ~ ("."~> parallel) ^^ { // objective move
      case (amb, name) ~ (par, free) =>
        `go.`(amb, par) -> (name ++ free)
    } |
    IDENT ~ ("{"~>rep1sep(name, ",")<~"}") ^^ { // pointed values
      case id ~ pointers =>
        `{}`(id, pointers.map(_._2).reduce(_ ++ _)) -> Names()
    } |
    agent() | // invocation
    expansion

  def expansion: Parser[(`[|]`, Names)] =
    regexMatch("""\[(\d*)\|""".r) >> { m =>
      val grp1 = m.group(1)
      val code = if grp1.isEmpty then 0 else grp1.toInt
      (expand(defn(code), s"|$grp1]") <~ s"|$grp1]") ~ opt( ("{"~>rep1sep(name, ",")<~"}") )
    } ^^ {
      case (it @ `[|]`(Encoding(_, _, const, bound), _, _), free) ~ Some(_pointers) =>
        val pointers = _pointers.map(_._2).reduce(_ ++ _)
        val assign = bound zip pointers
        given MutableList[(String, String)]()
        it.copy(assign = Some(assign)).rename -> (free ++ const)
      case (it @ `[|]`(Encoding(_, _, const, _), _, _), free) ~ _ =>
        it -> (free ++ const)
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
    "ν"~>"("~>rep1sep(name, ",")<~")" ^^ { ns => // restriction
      ν(ns.map(_._1)*) -> (ns.map(_._2).reduce(_ ++ _), Names())
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
    ("("~>name<~")") ~ opt( expression ) <~ "." ^^ { // input action
      case _ ~ Some((Left(enums), _)) =>
        throw TermParsingException(enums)
      case (name, binding) ~ Some((Right(it), free2)) =>
        `()`(Some(it), name) -> (binding, free2)
      case (name, binding) ~ _ =>
        `()`(None, name) -> (binding, Names())
    }

  def agent(binding: Boolean = false): Parser[(`(*)`, Names)] =
    qual ~ IDENT ~ opt( "("~>rep1sep(name, ",")<~")" ) ^^ {
      case qual ~ id ~ _ if binding && qual.nonEmpty =>
        throw EquationQualifiedException(id, qual)
      case qual ~ "Self" ~ Some(params) =>
        self += code
        `(*)`("Self_" + code, qual, params.map(_._1)*) -> params.map(_._2).reduce(_ ++ _)
      case qual ~ "Self" ~ _ =>
        self += code
        `(*)`("Self_" + code, qual) -> Names()
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

  type Bind = (`(*)`, `|`)

  type Define = (Encoding, `|`)

  case class Encoding(code: Int, term: Term, const: Names, bound: Names):
    override def toString: String =
      if code == 0 then s"[| $term |]" else s"[$code| $term |$code]"

  sealed trait AST extends Any

  case class `|`(components: `.`*) extends AST:
    override def toString: String = components.mkString(" | ")

  object ∅ extends `|`():
    override def canEqual(that: Any): Boolean =
      that.isInstanceOf[`|`]

    override def equals(any: Any): Boolean = any match
      case that: `|` => that.components.size == 0
      case _ => false

    override def toString: String = "()"

  case class `.`(end: `&`, prefixes: Pre*) extends AST:
    override def toString: String =
      prefixes.mkString(" ") + (if prefixes.isEmpty then "" else " ") + (if ∅ != end && end.isInstanceOf[`|`]
                                                                         then "(" + end + ")" else end)

  sealed trait Pre extends Any

  case class ν(names: String*) extends AnyVal with Pre: // forcibly
    override def toString: String = names.mkString("ν(", ", ", ")")

  case class τ(code: Option[Either[List[Enumerator], Term]]) extends AnyVal with Pre:
    override def toString: String = "τ."

  case class `,.`(path: Ambient.AST*) extends Pre:
    override def toString: String = path.mkString("", ", ", ".")

  case class `()`(code: Option[Term], name: String) extends Pre:
    override def toString: String = s"($name)."

  case class `<>`(code: Option[Term], path: Ambient.AST*) extends AST:
    override def toString: String = path.mkString("<", ", ", ">")

  case class `!`(guard: Option[String], par: `|`) extends AST:
    override def toString: String = "!" + guard.map("." + _).getOrElse("") + par

  case class `[]`(amb: String, par: `|`) extends AST:
    override def toString: String = amb + (if ∅ == par then " [ ]" else " [ " + par + " ]")

  case class `go.`(amb: String, par: `|`) extends AST:
    override def toString: String = "go " + amb + "." + par

  case class `[|]`(encoding: Encoding,
                   par: `|`,
                   assign: Option[Set[(String, String)]] = None) extends AST:
    override def toString: String =
      s"""$encoding${assign.map{_.map(_ + "->" + _).mkString("{", ", ", "}")}.getOrElse("")} = $par"""

  case class `{}`(identifier: String,
                  pointers: Names) extends AST:
    override def toString: String = s"""$identifier{${pointers.mkString(", ")}}"""

  case class `(*)`(identifier: String,
                   qual: List[String],
                   params: String*) extends AST:
    override def toString: String = s"$identifier(${params.mkString(", ")})"


  // exceptions

  import Expression.ParsingException

  class EquationParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class EquationQualifiedException(id: String, qual: List[String])
      extends EquationParsingException(s"A qualified package ${qual.mkString(".")} is present in the left hand side of $id")

  case class EquationFreeNamesException(id: String, free: Names)
      extends EquationParsingException(s"The free names (${free.mkString(", ")}) in the right hand side are not formal parameters of the left hand side of $id")

  case class DefinitionFreeNamesException(code: Int, free: Names)
      extends EquationParsingException(s"The free names (${free.mkString(", ")}) in the right hand side are not formal parameters of the left hand side of encoding $code")

  import scala.meta.Enumerator

  case class TermParsingException(enums: List[Enumerator])
      extends ParsingException(s"The embedded Scalameta should be a Term, not Enumerator `$enums'")


  // functions

  extension[T <: AST](ast: T)

    def flatten: T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case `∅` => ∅

        case `|`(`.`(par: `|`), it*) =>
          val lhs = par.flatten
          val rhs = `|`(it*).flatten
          `|`((lhs.components ++ rhs.components).filterNot(∅ == `|`(_))*)

        case `|`(seq, it*) =>
          val lhs = `|`(seq.flatten)
          val rhs = `|`(it*).flatten
          `|`((lhs.components ++ rhs.components).filterNot(∅ == `|`(_))*)

        case `.`(`|`(`.`(end, ps*)), it*) =>
          `.`(end, (it ++ ps)*).flatten

        case `.`(end, it*) =>
          `.`(end.flatten, it*)

        case `!`(None, par) =>
          par.flatten match
            case `|`(`.`(end: `!`)) => end
            case it => `!`(None, it)

        case `!`(guard, par) =>
          `!`(guard, par.flatten)

        case `[]`(amb, par) =>
          `[]`(amb, par.flatten)

        case `go.`(amb, par) =>
          `go.`(amb, par.flatten)

        case `[|]`(encoding, par, assign) =>
          `[|]`(encoding, par.flatten, assign)

        case _ => ast

    def capitals: Names =

      ast match

        case `∅` => Set.empty

        case `|`(it*) => it.map(_.capitals).reduce(_ ++ _)

        case `.`(end, _*) =>
          end.capitals

        case `!`(_, par) =>
          par.capitals

        case `[]`(_, par) =>
          par.capitals

        case `go.`(_, par) =>
          par.capitals

        case `[|]`(_, par, _) =>
          par.capitals

        case `{}`(id, _) => Set(id)

        case `(*)`(id, Nil) => Set(id)

        case _ => Set.empty
