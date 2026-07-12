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

import scala.meta.Term

import Expression.Code
import Ambient.*
import Calculus.*
import Encoding.*
import scala.util.parsing.combinator.masc.parser.Expansion.Duplications


abstract class Calculus extends Ambient:

  def equation(using Duplications): Parser[Bind] =
    invocation(true)<~"=" >> {
      case (bind, bound) =>
        _code = -1
        _dir = None
        given Bindings = Bindings() ++ bound.map(_ -> Occurrence(None, pos()))
        given Int = 1
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

  def parallelʹ(using Bindings, Duplications, Int): Parser[(∥, Names)] =
    opt( "("~>parallel<~")" ) ^^ { _.getOrElse(∅() -> Names()) }

  def parallel(using Bindings, Duplications, Int): Parser[(∥, Names)] =
    scale >> { scaling =>
      val scalingʹ = scaling.abs
      given Int = if scalingʹ == 1 then summon[Int] else scalingʹ
      rep1sep(sequential, "|") ^^ { _.unzip match
        case (it, ns) =>
          if scalingʹ == 0
          then
            ∅() -> Names()
          else if _scaling && emitter.canScale
          then
            ∥(scaling, it*) -> ns.reduce(_ ++ _)
          else
            ∥(-1, List.fill(scalingʹ)(it).reduce(_ ++ _).toSeq*) -> ns.reduce(_ ++ _)
      }
    }

  def sequential(using bindings: Bindings)(using Duplications, Int): Parser[(`.`, Names)] =
    given Bindings = Bindings(bindings)
    prefixes ~ ( leaf | parallelʹ ) ^^ {
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
    "ν"~>"("~>names<~")" ^^ { _.unzip match // restriction
      case (ns, bs) =>
        val bound = bs.reduce(_ ++ _)
        BindingOccurrence(bound)
        ν(ns*) -> (bound, Names())
    } |
    "τ" ~> opt( expression ) <~ "." ^^ { // silent transition
      case Some((it, free)) =>
        PendingOccurrence(free)
        τ(Some(it)) -> (Names(), free)
      case _ =>
        τ(None) -> (Names(), Names())
    } |
    caps <~ "." ^^ { // capability action
      case (path, free) =>
        PendingOccurrence(free)
        `..`(path*) -> (Names(), free)
    } |
    ("("~>name<~")") ~ opt( expression ) <~ "." ^^ {
      case _ ~ Some(((Left(enums), _), _)) =>
        throw TermParsingException(enums)
      case (name, bound) ~ Some((it, free)) =>
        PendingOccurrence(free)
        BindingOccurrence(bound)
        `()`(name, Some(it)) -> (bound, free)
      case (name, bound) ~ _ =>
        BindingOccurrence(bound)
        `()`(name, None) -> (bound, Names())
    }

  def leaf(using Bindings, Duplications, Int): Parser[(-, Names)] =
    "!"~> scale ~ opt( pace ) ~ opt( "."~> "("~>name<~")" <~"." ) >> { // [guarded] replication
      case parallelism ~ pace ~ Some((it, bound)) =>
        BindingOccurrence(bound)
        parallel ^^ {
          case (par, free) =>
            `!`(parallelism, pace, Some(it), par) -> (free &~ bound)
        }
      case parallelism ~ pace ~ _ =>
        parallel ^^ {
          case (par, free) =>
            `!`(parallelism, pace, None, par) -> free
        }
    } |
    name >> { // ambient
      case (amb, name) =>
        PendingOccurrence(name)
        "["~> parallel <~"]" ^^ {
          case (par, free) =>
            `[]`(amb, par) -> (name ++ free)
        }
    } |
    ("<"~> opt( caps ) <~">") ~ opt( expression ) ^^ { // output action
      case _ ~ Some(((Left(enums), _), _)) =>
        throw TermParsingException(enums)
      case Some((path, free)) ~ Some((it @ (Right(_), _), freeʹ)) =>
        PendingOccurrence(free ++ freeʹ)
        <>(Some(it), path*) -> (free ++ freeʹ)
      case Some((path, free)) ~ _ =>
        PendingOccurrence(free)
        <>(None, path*) -> free
      case _ =>
        <>(None) -> Names()
    } |
    "go"~> name <~"."  >> { // objective move
      case (amb, name) =>
        PendingOccurrence(name)
        parallel ^^ {
          case (par, free) =>
            `go.`(amb, par) -> (name ++ free)
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

  def invocation(equation: Boolean = false): Parser[(`(*)`, Names)] =
    qual ~ IDENT ~ opt( "("~> names ~ opt(if equation then "*" else "") <~")" ) ^^ {
      case qual ~ identifier ~ _ if equation && qual.nonEmpty =>
        throw EquationQualifiedException(identifier, qual)
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

  type Bind = (`(*)`, ∥)

  export Pre.*
  export AST.*

  enum Pre:

    case ν(names: String*) // forcibly

    case τ(code: Option[Code])

    case `..`(path: Ambient.AST*)

    case `()`(name: String, code: Option[Code])

    override def toString: String = this match
      case ν(names*) => names.mkString("ν(", ", ", ")")
      case `..`(path*) => path.mkString("", ". ", ".")
      case `()`(name, _) => s"($name)."
      case _ => "τ."

  enum AST:

    case ∥(scaling: Int, components: AST.`.`*)

    case `.`(end: AST.∥ | -, prefixes: Pre*)

    case <>(code: Option[Code], path: Ambient.AST*)

    case !(parallelism: Int,
           pace: Option[(Long, String)],
           guard: Option[String],
           par: AST.∥)

    case `[]`(amb: String, par: AST.∥)

    case `go.`(amb: String, par: AST.∥)

    case `⟦⟧`(definition: Definition,
              par: AST.∥,
              xid: String = null,
              pointers: List[String] = Nil)

    case `{}`(identifier: String,
              pointers: List[String],
              agent: Boolean = false,
              params: String*)

    case `(*)`(identifier: String,
               qual: List[String],
               params: String*)

    override def toString: String = this match
      case ∅() => "()"
      case ∥(-1, components*) => components.mkString(" | ")
      case ∥(sc, components*) => sc + " * " + components.mkString(" | ")

      case `.`(∅()) => "()"
      case `.`(∅(), prefixes*) => prefixes.mkString(" ") + " ()"
      case `.`(end: ∥, prefixes*) =>
        prefixes.mkString(" ") + (if prefixes.isEmpty then "" else " ") + "(" + end + ")"
      case `.`(end, prefixes*) =>
        prefixes.mkString(" ") + (if prefixes.isEmpty then "" else " ") + end

      case <>(_, path*) => path.mkString("<", ". ", ">")

      case !(_, _, guard, par) => "!" + guard.map(".(" + _ + ").").getOrElse("") + par

      case `[]`(amb, ∅()) => amb + " [ ]"
      case `[]`(amb, par) => amb + " [ " + par + " ]"

      case `go.`(amb, par) => "go " + amb + "." + par

      case `⟦⟧`(Definition(code, term, constants, variables, _), par, _, pointers) =>
        val assignment = if (variables.isEmpty)
                         then
                           ""
                         else {
                           (variables zip pointers).map { (l, r) => s"$l = $r" }
                         ++ variables.drop(pointers.size)
                         }.mkString("{", ", ", "}")
        if constants.isEmpty
        then
          s"""${Definition(code, term)}$assignment = $par"""
        else
          s"""${Definition(code, term)}${constants.mkString("(", ", ", ")")}$assignment = $par"""

      case `{}`(identifier, pointers, agent, params*) =>
        val ps = if agent then params.mkString("(", ", ", ")") else ""
        s"""$identifier$ps{${pointers.mkString(", ")}}"""

      case `(*)`(identifier, qual, params*) =>
        import emitter.shared.Meta.\
        val args = params.map(\(_)).toList
        val term = qual match
          case h :: t => (t.map(\(_)) :+ \("π") :+ \(identifier)).foldLeft(h: Term)(Term.Select(_, _))
          case _ => \(identifier)
        Term.Apply(term, Term.ArgClause(args)).toString

  object ∅ :
    def apply(): ∥ = ∥(-1)
    def unapply(self: AST): Boolean = self match
      case par: ∥ => par.isVoid
      case _ => false

  given `_∥_`: {} with
    extension (self: ∥)
      def copy(scaling: Int = self.scaling,
               components: Seq[`.`] = self.components): ∥ =
        ∥(scaling, components*)

  given `_._`: {} with
    extension (self: `.`)
      def copy(end: ∥ | - = self.end,
               prefixes: Seq[Pre] = self.prefixes): `.` =
        `.`(end, prefixes*)

  given `_<>_`: {} with
    extension (self: <>)
      def copy(code: Option[Code] = self.code,
               path: Seq[Ambient.AST] = self.path): <> =
        <>(code, path*)

  given `_{}_`: {} with
    extension (self: `{}`)
      def copy(identifier: String = self.identifier,
               pointers: List[String] = self.pointers,
               agent: Boolean = self.agent,
               params: Seq[String] = self.params): `{}` =
        `{}`(identifier, pointers, agent, params*)

  given `_(*)_`: {} with
    extension (self: `(*)`)
      def copy(identifier: String = self.identifier,
               qual: List[String] = self.qual,
               params: Seq[String] = self.params): `(*)` =
        `(*)`(identifier, qual, params*)


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

  extension (par: ∥)
    def isVoid: Boolean = par match
      case ∥(_) => true
      case _ => par.components.forall { case `.`(par: ∥) => par.isVoid case _ => false }

  extension [T <: AST](ast: T)

    def foreach(g: AST => Unit)(h: PartialFunction[AST, Unit] = PartialFunction.empty): Unit =

      h.applyOrElse(ast, {

        case ∅() =>

        case ∥(_, components*) =>
          components.foreach(_.foreach(g)(h))

        case `.`(end, _*) =>
          end.foreach(g)(h)

        case !(_, _, _, par) =>
          par.foreach(g)(h)

        case `[]`(_, par) =>
          par.foreach(g)(h)

        case `go.`(_, par) =>
          par.foreach(g)(h)

        case `⟦⟧`(_, par, _, _) =>
          par.foreach(g)(h)

        case _ => g(ast)

      })

    def mapreduce[R](g: AST => R)(h: (R, R) => R): R =

      ast match

        case ∅() => g(ast)

        case ∥(_, components*) =>
          components.map(_.mapreduce(g)(h)).reduce(h)

        case it @ `.`(end, _*) =>
          h(g(it), end.mapreduce(g)(h))

        case it @ !(_, _, _, par) =>
          h(g(it), par.mapreduce(g)(h))

        case it @ `[]`(_, par) =>
          h(g(it), par.mapreduce(g)(h))

        case it @ `go.`(_, par) =>
          h(g(it), par.mapreduce(g)(h))

        case it @ `⟦⟧`(_, par, _, _) =>
          h(g(it), par.mapreduce(g)(h))

        case _ => g(ast)

    def map(g: AST => AST)(h: AST => AST = identity): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case it @ ∥(_, components*) =>
          it.copy(components = components.map(_.map(g)(h)))

        case it @ `.`(end, _*) =>
          h(it.copy(end = end.map(g)(h)))

        case it @ !(_, _, _, par) =>
          h(it.copy(par = par.map(g)(h)))

        case it @ `[]`(_, par) =>
          h(it.copy(par = par.map(g)(h)))

        case it @ `go.`(_, par) =>
          h(it.copy(par = par.map(g)(h)))

        case it @ `⟦⟧`(_, par, _, _) =>
          h(it.copy(par = par.map(g)(h)))

        case _ => h(ast)

    def mapʹ(g: AST => AST)(h: AST => AST): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case it @ ∥(_, components*) =>
          it.copy(components = components.map(_.mapʹ(g)(h)))

        case it: `.` =>
          val itʹ @ `.`(end, _*) = h(it)
          itʹ.copy(end = end.mapʹ(g)(h))

        case it: ! =>
          val itʹ @ !(_, _, _, par) = h(it)
          itʹ.copy(par = par.mapʹ(g)(h))

        case it: `[]` =>
          val itʹ @ `[]`(_, par) = h(it)
          itʹ.copy(par = par.mapʹ(g)(h))

        case it: `go.` =>
          val itʹ @ `go.`(_, par) = h(it)
          itʹ.copy(par = par.mapʹ(g)(h))

        case it: `⟦⟧` =>
          val itʹ @ `⟦⟧`(_, par, _, _) = h(it)
          itʹ.copy(par = par.mapʹ(g)(h))

        case _ => h(ast)

    def mapʹʹ(g: AST => AST)(h: AST => (AST, Boolean)): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case it @ ∥(_, components*) =>
          it.copy(components = components.map(_.mapʹʹ(g)(h)))

        case it: `.` =>
          h(it) match
            case (itʹ @ `.`(end, _*), false) =>
              itʹ.copy(end = end.mapʹʹ(g)(h))
            case (itʹ, _) => itʹ

        case it: ! =>
          h(it) match
            case (itʹ @ !(_, _, _, par), false) =>
              itʹ.copy(par = par.mapʹʹ(g)(h))
            case (itʹ, _) => itʹ

        case it: `[]` =>
          h(it) match
            case (itʹ @ `[]`(_, par), false) =>
              itʹ.copy(par = par.mapʹʹ(g)(h))
            case (itʹ, _) => itʹ

        case it: `go.` =>
          h(it) match
            case (itʹ @ `[]`(_, par), false) =>
              itʹ.copy(par = par.mapʹʹ(g)(h))
            case (itʹ, _) => itʹ

        case it: `⟦⟧` =>
          h(it) match
            case (itʹ @ `⟦⟧`(_, par, _, _), false) =>
              itʹ.copy(par = par.mapʹʹ(g)(h))
            case (itʹ, _) => itʹ

        case _ => h(ast)._1

    def flatten: T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() =>
          ∅()

        case it @ ∥(_, `.`(par: ∥), components*) =>
          val lhs = par.flatten
          val rhs = ∥(-1, components*).flatten
          it.copy(components = (lhs.components ++ rhs.components).filterNot(∥(-1, _).isVoid))

        case it @ ∥(_, seq, components*) =>
          val lhs: ∥ = ∥(-1, seq.flatten)
          val rhs = ∥(-1, components*).flatten
          it.copy(components = (lhs.components ++ rhs.components).filterNot(∥(-1, _).isVoid))

        case `.`(∥(_, `.`(end, psr*)), psl*) =>
          `.`(end, (psl ++ psr)*).flatten

        case it @ `.`(end, _*) =>
          it.copy(end = end.flatten)

        case !(-1, None, None, par) =>
          par.flatten match
            case ∥(-1|1, `.`(end: !)) => end
            case it => `!`(-1, None, None, it)

        case it @ !(_, _, _, par) =>
          it.copy(par = par.flatten)

        case it @ `[]`(_, par) =>
          it.copy(par = par.flatten)

        case it @ `go.`(_, par) =>
          it.copy(par = par.flatten)

        case _ => ast
