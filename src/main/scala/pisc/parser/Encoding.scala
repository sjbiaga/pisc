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

import scala.collection.mutable.{
  LinkedHashMap => Map,
  ListBuffer => MutableList,
  LinkedHashSet => Set
}

import scala.meta.Term

import helper.υidυ.rewrite

import Expression.Code
import Pi.*
import Calculus.*
import Encoding.*
import scala.util.parsing.combinator.pisc.parser.Expansion.Duplications


abstract class Encoding extends Calculus:

  def definition(using Duplications): Parser[Option[Define]] =
    template ~ opt( "("~>names<~")" ) ~ opt( pointers ) >> {
      case _ if _directive.isDefined =>
        Directive(_directive.get, emitter, _settings)()
        ".*".r ^^ { _ => None }
      case _ if _settings.exclude =>
        ".*".r ^^ { _ => None }
      case (term, _parameters) ~ _constants ~ _variables =>
        val parameters = _parameters.filterNot(_.name.charAt(0).isUpper)
        val constants = _constants.map(_.map(_._2).reduce(_ ++ _)).getOrElse(Names())
        val variables = _variables.map(_._2).getOrElse(Names())
        if (parameters & constants).nonEmpty
        || (constants & variables).nonEmpty
        || (variables & parameters).nonEmpty
        then
          throw DefinitionParametersException(_code)
        val bound = _parameters ++ constants ++ variables
        given Bindings = Bindings() ++
                         bound
                           .filterNot(_.name.charAt(0).isUpper)
                           .map { it => it -> (if parameters.contains(it) then pos_() else pos()) }
                           .map(_ -> Occurrence(None, _))
        given Int = 1
        "="~> choice ^^ {
          case (_sum, _free) =>
            val sum = _sum.flatten
            val free = _free ++ sum.capitals
            if (free &~ bound).nonEmpty
            then
              throw DefinitionFreeNamesException(_code, free &~ bound)
            if parameters.size == _parameters.size
            then
              if !_settings.exclude
              then
                val bind: `(*)` = `(*)`("Self_" + _code, Nil, bound.map(λ(_)).toSeq*)
                eqtn :+= bind -> sum
            Some {
              Macro(parameters.toList, _parameters.size, constants, variables, given_Bindings, sum)
              ->
              Definition(_code, term, constants, variables, sum)
            }
        }
    }

  def instantiation(using bindings: Bindings, duplications: Duplications, _scaling: Int): Parser[(`⟦⟧`, Names)] =
    given Bindings = Bindings(bindings)
    regexMatch("""⟦(\d*)""".r) >> { m =>
      if _nest == 0 then _cache.clear()
      nest(true)
      val grp1 = m.group(1)
      val code = if grp1.isEmpty
                 then
                   val def1 = defn.filter { (_, it) => it.size == 1 && it.head._2.term.isEmpty }
                   if def1.size == 1
                   then def1.head._2.head._2.code
                   else -1
                 else
                   grp1.toInt
      defn.get(code) match {
        case Some(it) => it
        case _ if grp1.nonEmpty || defn.isEmpty =>
          throw NoDefinitionException(code max 0)
        case _ =>
          defn.values.reduce(_ ++ _).filterNot(_._2.term.isEmpty)
      } match
        case ((_, definition @ Definition(_, None, _, _, _))) :: Nil =>
          choice <~ s"$grp1⟧" ^^ {
            case (sum, free) =>
              val xid = χ_id
              duplications += xid -> (false, Map())
              `⟦⟧`(definition, sum.flatten, xid) -> free
          }
        case it =>
          instance(it, s"$grp1⟧") <~ s"$grp1⟧"
    } >> {
      case (exp @ `⟦⟧`(Definition(code, _, constants, variables, _), _, _, _), free) =>
        opt( pointers ) ^^ { _.getOrElse(Nil -> Names()) } ^^ { (pointersʹ, freeʹ) =>
          if pointersʹ.size > variables.size
          then
            warn(throw TooManyPointersParsingException(code, pointersʹ.size - variables.size))
          try
            given MutableList[(Symbol, Symbol)]()
            val expʹ = exp.rename()(id)
            nest(false)
            bindings ++= purged
            PendingOccurrence(freeʹ)(using bindings)
            expʹ.copy(pointers = pointersʹ) -> (free ++ constants ++ freeʹ)
          catch
            case it: NoBPEx => throw NoBindingParsingException(_code, _nest, it.getMessage)
            case it => throw it
        }
    }

  def instance(defs: List[Define], end: String)
              (using Bindings, Duplications, Int): Parser[(`⟦⟧`, Names)]

  def pointers: Parser[(List[Symbol], Names)] =
    "{"~>names<~"}" ^^ {
      case ps if !ps.forall(_._1.isSymbol) =>
        throw PointersParsingException(ps.filterNot(_._1.isSymbol).map(_._1)*)
      case ps => ps.unzip match
        case (λs, ns) =>
          λs.map(_.asSymbol) -> ns.reduce(_ ++ _)
    }

  def capital: Parser[(`{}`, Names)] =
    IDENT ~ pointers ^^ {
      case identifier ~ ps =>
        `{}`(identifier, ps._1) -> ps._2
    } |
    IDENT <~"{"<~"}" ^^ (`{}`(_, Nil) -> Names()) |
    IDENT ~ ("("~>opt( names )<~")") ~ pointers ^^ {
      case identifier ~ Some(params) ~ ps =>
        `{}`(identifier, ps._1, true, params.map(_._1)*) -> (ps._2 ++ params.map(_._2).reduce(_ ++ _))
      case identifier ~ _ ~ ps =>
        `{}`(identifier, ps._1, true) -> ps._2
    } |
    IDENT ~ ("("~>opt( names )<~")") <~"{"<~"}" ^^ {
      case identifier ~ Some(params) =>
        `{}`(identifier, Nil, true, params.map(_._1)*) -> params.map(_._2).reduce(_ ++ _)
      case identifier ~ _ =>
        `{}`(identifier, Nil, true) -> Names()
    }

  protected final val _cache = Map[CacheKey, CacheValue]()


object Encoding:

  type Define = (Macro, Definition)

  type Fresh = (Definition, (Int, List[Option[Symbol]]))

  type CacheKey = ((Seq[Long], (String, Either[String, String])), Int)

  private type CacheValue = (+ | `⟦⟧`, (Any, Any), Names, Bindings, Encoding#Input)

  case class Macro(parameters: List[Symbol],
                   arity: Int,
                   constants: Names,
                   variables: Names,
                   bindings: Bindings,
                   sum: +):
    def apply(code: Int, term: Term, dups: Boolean)
             (id: => String, χ_id: => String)
             (using Duplications): Fresh =
      given Bindings = Bindings(bindings)
      given MutableList[(Symbol, Symbol)]()
      val variablesʹ = variables
        .map { it =>
          val υidυ = it.rewrite(id)
          given_Bindings(υidυ) = given_Bindings(it)
          given_Bindings -= it
          given_MutableList_Symbol_Symbol.append(it -> υidυ)
          υidυ
        }
      val sumʹ = sum.rename(dups, collect = true)(id, χ_id)
      val shadows = (
        parameters.map(_ -> None).toMap
        ++
        purged.collect { case (it, Binder(υidυ)) => it -> Some(υidυ) }
      ) .toList
        .sortBy { (it, _) => parameters.indexOf(it) }
        .map(_._2)
      Definition(code, Some(term), constants, variablesʹ, sumʹ)
      ->
      (arity - shadows.count(_.isDefined) -> shadows)

  case class Definition(code: Int,
                        term: Option[Term],
                        constants: Names,
                        variables: Names,
                        sum: +):
    def apply(_code: Int, nest: Int, dups: Boolean,
              duplicated: (Bindings, Duplications) ?=> String => Term => Unit,
              replace: (+ | `⟦⟧` => + | `⟦⟧`) ?=> + => +)
             (id: => String)
             (using duplications: Duplications)
             (using Bindings): `⟦⟧` =
      if dups
      then
        val ids = MutableList[String]()
        val idsʹ = MutableList.from {
          duplications.flatMap {
            case (xid, (true, _)) => Some(xid)
            case _ => None
          }
        }
        given (+ | `⟦⟧` => + | `⟦⟧`) = { ast =>
          def count(ast: AST): Unit =
            ast.foreach(count) {
              case it: `⟦⟧` if ids.contains(it.xid) =>
                duplications += it.xid -> (true -> duplications(it.xid)._2)
                count(it.sum)
              case it: `⟦⟧` if idsʹ.contains(it.xid) =>
                count(it.sum)
              case it: `⟦⟧` =>
                ids += it.xid
                count(it.sum)
            }
          count(ast)
          try
            given MutableList[(Symbol, Symbol)]()
            ast.rename(dups, duplicated)(id)
          catch
            case t: NoBPEx => throw NoBindingParsingException(_code, nest, t.getMessage)
            case t => throw t
        }
        def reset(ast: AST): Unit =
          ast.foreach(reset) {
            case it: `⟦⟧` if ids.contains(it.xid) =>
              duplications += it.xid -> (false -> duplications(it.xid)._2)
              reset(it.sum)
          }
        val exp: `⟦⟧` = `⟦⟧`(this, replace(sum))
        reset(exp.sum)
        exp
      else
        given (+ | `⟦⟧` => + | `⟦⟧`) = { ast =>
          try
            given MutableList[(Symbol, Symbol)]()
            ast.rename(dups, duplicated)(id)
          catch
            case t: NoBPEx => throw NoBindingParsingException(_code, nest, t.getMessage)
            case t => throw t
        }
        `⟦⟧`(this, replace(sum))

    override def toString: String = Definition(code, term)
      + (if constants.isEmpty then "" else constants.map(_.name).mkString("(", ", ", ")"))
      + (if variables.isEmpty then "" else variables.map(_.name).mkString("{", ", ", "}"))
      + " = " + sum

  object Definition:

    def apply(code: Int, term: Option[Term]): String =
      term match
        case Some(term) => if code == 0 then s"⟦ $term ⟧" else s"⟦$code $term $code⟧"
        case _ => if code == 0 then s"⟦ ⟧" else s"⟦$code $code⟧"

  final case class Position(counter: Long,
                            binds: Boolean,
                            path: Seq[Long] = Nil)

  final case class Occurrence(shadow: Symbol | Option[Symbol],
                              position: Position,
                              pending: Boolean = false):
    val aliasing = position.binds && position.counter < 0

  object Binder:
    def apply(self: Occurrence)(υidυ: Symbol) = self.copy(shadow = υidυ)
    def unapply(self: Occurrence): Option[Symbol] =
      self.shadow match
        case it: Symbol => Some(it)
        case _ => None

  object Shadow:
    def apply(self: Occurrence)(υidυ: Symbol) = self.copy(shadow = Some(υidυ))
    def unapply(self: Occurrence): Option[Symbol] =
      self.shadow match
        case it @ Some(_) => it
        case _ => None

  type Bindings = Map[Symbol, Occurrence]

  object Bindings:
    def apply(): Bindings = Map()
    def apply(bindings: Bindings): Bindings = Map.from(bindings)


  // exceptions

  import Expression.ParsingException

  class PointersParsingException(names: λ*)
      extends PrefixChannelsParsingException(names*)

  case class NoDefinitionException(code: Int)
      extends ParsingException(s"No definition for encoding $code")

  case class DefinitionParametersException(code: Int)
      extends EquationParsingException(s"The parameters, constants, and variables must all be different in the left hand side of definition $code")

  case class DefinitionFreeNamesException(code: Int, free: Names)
      extends EquationParsingException(s"""The free names (${free.map(_.name).mkString(", ")}) in the right hand side are not formal parameters in the left hand side of definition $code""")

  abstract sealed class BindingParsingException(code: Int, nest: Int, msg: String, cause: Throwable = null)
      extends ParsingException(msg
                                 + s" at nesting level #$nest"
                                 + (if code >= 0 then s" in the right hand side of definition $code" else ""), cause)

  case class NoBindingParsingException(code: Int, nest: Int, name: String)
      extends BindingParsingException(code, nest, s"No binding for $name")

  final private class NoBPEx(name: String) extends Throwable(name)

  case class TooManyPointersParsingException(code: Int, amount: Int)
      extends ParsingException(s"""Too many pointers (+$amount) assigned to definition $code""")

  case class UniquenessBindingParsingException(code: Int, nest: Int, name: Symbol, hardcoded: Boolean, how: String)
      extends BindingParsingException(code, nest, s"""A binding name (${name.name}) does not correspond to a unique ${if hardcoded then "hardcoded" else "encoded"} binding occurrence, being $how""")

  case class ScopeBindingParsingException(code: Int, nest: Int, name: Symbol)
      extends BindingParsingException(code, nest, s"""An occurrence of a definition parameter (${name.name}) is not in the scope of its binding occurrence""")


  // functions

  def renamed(it: Symbol)
             (using refresh: MutableList[(Symbol, Symbol)])
             (using bindings: Bindings): Symbol =
    refresh.find(_._1 == it) match
      case Some((_, r)) => r
      case _ =>
        bindings.find { case (`it`, Binder(_) | Shadow(_)) => true case _ => false } match
          case Some((_, Binder(it))) => it
          case Some((_, Shadow(it))) => it
          case _ =>
            bindings.find { case (`it`, _) | (_, Shadow(`it`)) => true case _ => false } match
              case Some(_) => it
              case _ => throw NoBPEx(it.name)

  def renamed(term: Term)
             (using MutableList[(Symbol, Symbol)])
             (using Bindings): Term =
    Expression(term)._1

  def recoded(using code: Option[Code])
             (using MutableList[(Symbol, Symbol)])
             (using Bindings): Option[Code] =
    code.map { (_, orig) =>
      val term = Expression(orig)._1
      Expression.recode(term)
    }

  def purged(using bindings: Bindings): Bindings =
    bindings.flatMap {
      case (name, Shadow(it)) =>
        bindings.find { case (`it`, Binder(_) | Shadow(_)) => true case _ => false } match
          case Some((_, occurrence)) =>
            Some(name -> (it -> occurrence))
          case _ =>
            None
      case _ =>
        None
    }.foreach {
      case (name, (it, occurrence)) =>
        bindings -= it
        bindings += name -> occurrence
    }
    cleaned

  inline def cleaned(using bindings: Bindings): Bindings =
    bindings.filter((_, it) => it.pending || it.aliasing)


  given Conversion[Symbol | Term, λ] = λ(_)

  extension [T <: AST](ast: T)

    def capitals: Names =

      ast.mapreduce {

        case `{}`(identifier, _, false) => Set(Symbol(identifier))

        case _ => Names()

      }(_ ++ _)


    def rename(dups: Boolean = false,
               duplicated: (Bindings, Duplications) ?=> String => Term => Unit = { (_, _) ?=> { _ => { _ => } } },
               collect: Boolean = false)
              (id: => String, χ_id: => String = null)
              (using bindings: Bindings)
              (using duplications: Duplications)
              (using refresh: MutableList[(Symbol, Symbol)]): T =

      def rebind(it: Symbol): Symbol =
        val υidυ = it.rewrite(id)
        bindings.find { case (_, Shadow(`it`)) => true case _ => false } match
          case Some((_, occurrence)) if collect && occurrence.aliasing =>
            bindings += it -> Binder(occurrence)(υidυ)
          case Some((_, occurrence)) =>
            bindings += it -> Shadow(occurrence)(υidυ)
          case _ =>
            refresh.prepend(it -> υidυ)
        υidυ

      inline def rename[S <: AST](ast: S): S =
        ast.rename(dups, duplicated, collect)(id, χ_id)

      given Conversion[AST, (T, Boolean)] = _.asInstanceOf[T] -> false
      import parser.Calculus.given

      ast.mapʹʹ(rename(_)) {

        case `.`(end, prefixes*) =>
          val n = refresh.size
          val prefixesʹ = prefixes.map {
            case ν(_names*) =>
              val names = _names.map(Symbol(_)).map(rebind(_))
              ν(names.map(_.asSymbol.name)*)
            case it @ τ(given Option[Code]) =>
              it.copy(code = recoded)
            case it @ π(λ(ch: Symbol), λ(params: List[`λ`]), Some(_), given Option[Code]) =>
              val paramsʹ = params.map {
                case par @ λ(Symbol("")) => par
                case λ(par: Symbol) => rebind(par)
              }
              it.copy(channel = renamed(ch), name = λ(paramsʹ), code = recoded)
            case it @ π(λ(ch: Symbol), λ(par: Symbol), Some(_), given Option[Code]) =>
              it.copy(channel = renamed(ch), name = rebind(par), code = recoded)
            case it @ π(λ(ch: Symbol), λ(arg: Symbol), None, given Option[Code]) =>
              it.copy(channel = renamed(ch), name = renamed(arg), code = recoded)
            case it @ π(λ(ch: Symbol), λ(term: Term), None, given Option[Code]) =>
              it.copy(channel = renamed(ch), name = renamed(term), code = recoded)
            case it @ π(λ(ch: Symbol), _, None, given Option[Code]) =>
              it.copy(channel = renamed(ch), code = recoded)
            case it => it
          }
          val endʹ = rename(end)
          refresh.dropInPlace(refresh.size - n)
          `.`(endʹ, prefixesʹ*) -> true

        case it @ ?:(((λ(lhs: Symbol), λ(rhs: Symbol)), m), _, _) =>
          it.copy(cond = ((renamed(lhs), renamed(rhs)), m))

        case it @ ?:(((λ(lhs: Symbol), rhs), m), _, _) =>
          it.copy(cond = ((renamed(lhs), rhs), m))

        case it @ ?:(((lhs, λ(rhs: Symbol)), m), _, _) =>
          it.copy(cond = ((lhs, renamed(rhs)), m))

        case it @ !(_, _, Some(τ @ τ(given Option[Code])), _) =>
          it.copy(guard = Some(τ.copy(code = recoded)))

        case it @ !(_, _, Some(π @ π(λ(ch: Symbol), λ(par: Symbol), Some(_), given Option[Code])), sum) =>
          val n = refresh.size
          val πʹ = π.copy(channel = renamed(ch), name = rebind(par), code = recoded)
          val sumʹ = rename(sum)
          refresh.dropInPlace(refresh.size - n)
          it.copy(guard = Some(πʹ), sum = sumʹ) -> true

        case it @ !(_, _, Some(π @ π(λ(ch: Symbol), λ(arg: Symbol), None, given Option[Code])), _) =>
          val πʹ = π.copy(channel = renamed(ch), name = renamed(arg), code = recoded)
          it.copy(guard = Some(πʹ))

        case it @ !(_, _, Some(π @ π(λ(ch: Symbol), λ(term: Term), None, given Option[Code])), _) =>
          val πʹ = π.copy(channel = renamed(ch), name = renamed(term), code = recoded)
          it.copy(guard = Some(πʹ))

        case it @ !(_, _, Some(π @ π(λ(ch: Symbol), _, None, given Option[Code])), _) =>
          val πʹ = π.copy(channel = renamed(ch), code = recoded)
          it.copy(guard = Some(πʹ))

        case it @ `⟦⟧`(dfn @ Definition(_, term, _, variables, _), sum, xid, pointers) =>
          if dups then term.foreach(duplicated(xid))
          val n = refresh.size
          val variablesʹ = variables
                       .map { it =>
                         val υidυ = it.rewrite(id)
                         refresh.prepend(it -> υidυ)
                         υidυ
                       }
          val sumʹ = rename(sum)
          refresh.dropInPlace(refresh.size - n)
          val dfnʹ = dfn.copy(variables = variablesʹ)
          val xidʹ =
            if dups && collect && term.isDefined
            then
              val υidυ = χ_id
              duplications += υidυ -> (false, duplications(xid)._2)
              duplications -= xid
              υidυ
            else
              xid
          val pointersʹ = pointers.map(renamed(_).asSymbol)
          it.copy(definition = dfnʹ, sum = sumʹ, xid = xidʹ, pointers = pointersʹ) -> true

        case it @ `{}`(_, pointers, _, params*) =>
          val pointersʹ = pointers.map(renamed(_).asSymbol)
          val paramsʹ: Seq[λ] = params
            .map {
              case λ(it: Symbol) => renamed(it)
              case it => it
            }
          it.copy(pointers = pointersʹ, params = paramsʹ)

        case it @ `(*)`(_, _, params*) =>
          val paramsʹ: Seq[λ] = params
            .map {
              case λ(it: Symbol) => renamed(it)
              case it => it
            }
          it.copy(params = paramsʹ)

        case it => it

      }
