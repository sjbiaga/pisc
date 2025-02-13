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

import scala.collection.mutable.{
  LinkedHashMap => Map,
  ListBuffer => MutableList,
  LinkedHashSet => Set
}

import scala.meta.Term

import Expression.Code
import Pi.*
import Calculus.*
import Encoding.*
import scala.util.parsing.combinator.pisc.parser.Expansion.{ replace, Substitution }


abstract class Encoding extends Calculus:

  def definition: Parser[Define] =
    template ~ opt( "("~>rep1sep(name, ",")<~")" ) ~ opt( pointers ) <~"=" >> {
      case (term, _parameters) ~ _constants ~ _variables =>
        val constants = _constants.map(_.map(_._2).reduce(_ ++ _)).getOrElse(Names())
        val variables = _variables.map(_._2).getOrElse(Names())
        val parameters = _parameters.filterNot(_.name.charAt(0).isUpper)
        if (parameters & constants).nonEmpty
        || (variables & parameters).nonEmpty
        || (constants & variables).nonEmpty
        then
          throw DefinitionParametersException(_code)
        val bound = _parameters ++ constants ++ variables
        given Bindings = Bindings() ++
                         bound
                           .filterNot(_.name.charAt(0).isUpper)
                           .map { it => it -> (if parameters.contains(it) then pos_() else pos()) }
                           .map(_ -> Occurrence(None, _))
        choice ^^ {
          case (_sum, _free) =>
            val sum = _sum.flatten
            val free = _free ++ sum.capitals
            if (free &~ bound).nonEmpty
            then
              throw DefinitionFreeNamesException(_code, free &~ bound)
            if parameters.size == _parameters.size
            then
              eqtn :+= `(*)`("Self_" + _code, Nil, bound.map(λ(_)).toSeq*) -> sum
            Macro(parameters.toList, _parameters.size, constants, variables, given_Bindings, sum)
            ->
            Definition(_code, term, constants, variables, sum)
        }
    }

  def instantiation(using bindings: Bindings): Parser[(`⟦⟧`, Names)] =
    given Bindings = Bindings(bindings)
    regexMatch("""⟦(\d*)""".r) >> { m =>
      if _nest == 0 then _cache.clear
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
          (choice <~ s"$grp1⟧") ~ opt( pointers ) ^^ {
            case (sum, free) ~ ps =>
              (`⟦⟧`(definition, definition.variables, sum.flatten) -> free) -> ps
          }
        case it =>
          (instance(it, s"$grp1⟧") <~ s"$grp1⟧") ~ opt( pointers ) ^^ {
            case exp ~ ps =>
              exp -> ps
          }
    } ^^ {
      case ((exp @ `⟦⟧`(Definition(_, _, constants, _, _), variables, _, _), free), _pointers) =>
        nest(false)
        given MutableList[(Symbol, λ)]()
        try
          val pointers = _pointers.map(_._1.map(renamed(_).asSymbol)).getOrElse(Nil)
          val assign = variables zip pointers
          given Names()
          val exp2 = exp.copy(assign = assign).rename(id, free)
          bindings ++= purged
          exp2 -> (free ++ constants)
        catch
          case it: NoBPEx => throw NoBindingParsingException(_code, _nest, it.getMessage)
          case it => throw it
    }

  def instance(defs: List[Define], end: String)
              (using Bindings): Parser[(`⟦⟧`, Names)]

  def pointers: Parser[(List[Symbol], Names)] =
    "{"~>rep1sep(name, ",")<~"}" ^^ {
      case ps if !ps.forall(_._1.isSymbol) =>
        throw PointersParsingException(ps.filterNot(_._1.isSymbol).map(_._1)*)
      case ps => ps.unzip match
        case (λs, ns) =>
          λs.map(_.asSymbol) -> ns.reduce(_ ++ _)
    }

  def capital: Parser[(`{}`, Names)] =
    IDENT ~ pointers ^^ {
      case id ~ ps =>
        `{}`(id, ps._1) -> ps._2
    } |
    IDENT <~"{"<~"}" ^^ (`{}`(_, Nil) -> Names()) |
    IDENT ~ ("("~>opt( rep1sep(name, ",") )<~")") ~ pointers ^^ {
      case id ~ Some(params) ~ ps =>
        `{}`(id, ps._1, true, params.map(_._1)*) -> (ps._2 ++ params.map(_._2).reduce(_ ++ _))
      case id ~ _ ~ ps =>
        `{}`(id, ps._1, true) -> ps._2
    } |
    IDENT ~ ("("~>opt( rep1sep(name, ",") )<~")") <~"{"<~"}" ^^ {
      case id ~ Some(params) =>
        `{}`(id, Nil, true, params.map(_._1)*) -> params.map(_._2).reduce(_ ++ _)
      case id ~ _ =>
        `{}`(id, Nil, true) -> Names()
    }

  protected final val _cache = Map[CacheKey, CacheValue]()


object Encoding:

  type Define = (Macro, Definition)

  type Fresh = (Definition, (Int, List[Option[Symbol]]))

  type CacheKey = ((Seq[Long], (String, Either[String, String])), Int)

  private type CacheValue = (`⟦⟧` | +, Any, Names, Bindings, Encoding#Input)

  case class Macro(parameters: List[Symbol],
                   arity: Int,
                   constants: Names,
                   variables: Names,
                   bindings: Bindings,
                   sum: +):
    def apply(code: Int, id: => String, term: Term): Fresh =
      given refresh: MutableList[(Symbol, λ)] = MutableList()
      val variables2 = variables
        .map { it =>
          val υidυ = Symbol(it.name.replaceAll("_υ.*υ", "") + id)
          refresh.prepend(it -> λ(υidυ))
          υidυ
        }
      given Bindings = Bindings(bindings)
      given Names()
      val sum2 = sum.rename(id, Set.empty, definition = true)
      val shadows = (
        parameters.map(_ -> None).toMap
        ++
        purged.collect { case (it, Binder(υidυ)) => it -> Some(υidυ) }
      ) .toList
        .sortBy { (it, _) => parameters.indexOf(it) }
        .map(_._2)
      Definition(code, Some(term), constants, variables2, sum2)
      ->
      (arity - shadows.count(_.nonEmpty) -> shadows)

  case class Definition(code: Int,
                        term: Option[Term],
                        constants: Names,
                        variables: Names,
                        sum: +):
    inline def apply()(using Substitution): `⟦⟧` =
      `⟦⟧`(this, variables, sum.replace.flatten)

    override def toString: String = Definition(code, term)
      + (if constants.isEmpty then "" else constants.map(_.name).mkString("(", ", ", ")"))
      + (if variables.isEmpty then "" else variables.map(_.name).mkString("{", ", ", "}"))
      + " = " + sum

  object Definition:

    def apply(code: Int, term: Option[Term]): String =
      term match
        case Some(term) => if code == 0 then s"⟦ $term ⟧" else s"⟦$code $term $code⟧"
        case _ => if code == 0 then s"⟦ ⟧" else s"⟦$code $code⟧"

  final case class Position(counter: Long, binds: Boolean)

  final case class Occurrence(shadow: Symbol | Option[Symbol], position: Position):
    val isBinding = position.binds && position.counter < 0

  object Binder:
    def apply(self: Occurrence)(υidυ: Symbol) = Occurrence(υidυ, self.position)
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
      extends EquationParsingException(s"The parameters, constants, and variables must all be different in the left hand side of encoding $code")

  case class DefinitionFreeNamesException(code: Int, free: Names)
      extends EquationParsingException(s"""The free names (${free.map(_.name).mkString(", ")}) in the right hand side are not formal parameters of the left hand side of encoding $code""")

  abstract class BindingParsingException(code: Int, nest: Int, msg: String, cause: Throwable = null)
      extends ParsingException(msg
                                 + s" at nesting level #$nest"
                                 + (if code >= 0 then s" in the right hand side of encoding $code" else ""), cause)

  case class NoBindingParsingException(code: Int, nest: Int, name: String)
      extends BindingParsingException(code, nest, s"No binding for $name")

  private class NoBPEx(name: String) extends Throwable(name)

  case class UniquenessBindingParsingException(code: Int, nest: Int, name: Symbol, hardcoded: Boolean)
      extends BindingParsingException(code, nest, s"""A binding name (${name.name}) does not correspond to a unique ${if hardcoded then "hardcoded" else "encoded"} binding occurrence, but is duplicated""")

  case class NonParameterBindingParsingException(code: Int, nest: Int, name: Symbol, hardcoded: Boolean)
      extends BindingParsingException(code, nest, s"""A binding name (${name.name}) in ${if hardcoded then "a hardcoded" else "an encoded"} binding occurrence does not correspond to a parameter""")


  // functions

  def renamed(it: Symbol)
             (using refresh: MutableList[(Symbol, λ)])
             (using bindings: Bindings): λ =
    refresh.find(_._1 == it) match
      case Some((_, r)) => r
      case _ =>
        bindings.find { case (`it`, Binder(_) | Shadow(_)) => true case _ => false } match
          case Some((_, Binder(it))) => λ(it)
          case Some((_, Shadow(it))) => λ(it)
          case _ =>
            bindings.find { case (`it`, _) | (_, Shadow(`it`)) => true case _ => false } match
              case Some(_) => λ(it)
              case _ => throw NoBPEx(it.name)

  def recoded(free: Names)
             (using code: Option[Code])
             (using MutableList[(Symbol, λ)])
             (using Bindings)
             (using bound: Names): Option[Code] =
    code.map { (_, orig) =>
      val term = Expression(orig)._1
      val (code2, names) = Expression.recode(term)
      free ++= names.filterNot(bound.contains(_))
      code2
    }

  def purged(using bindings: Bindings): Bindings =
    bindings.map {
      case (name, Shadow(it)) =>
        bindings.find { case (`it`, Binder(_) | Shadow(_)) => true case _ => false } match
          case Some((_, occurrence)) =>
            Some(name -> (it -> occurrence))
          case _ =>
            None
      case _ =>
        None
    }.foreach {
      case Some((name, (it, occurrence))) =>
        bindings -= it
        bindings += name -> occurrence
      case _ =>
    }
    binders

  inline def binders(using bindings: Bindings): Bindings =
    bindings.filter(_._2.isBinding)


  extension [T <: AST](ast: T)

    def capitals: Names =

      ast match

        case ∅(_) => Names()

        case +(it*) => it.map(_.capitals).reduce(_ ++ _)

        case ∥(it*) => it.map(_.capitals).reduce(_ ++ _)

        case `.`(end, _*) =>
          end.capitals

        case ?:(_, t, f) =>
          t.capitals ++ f.map(_.capitals).getOrElse(Names())

        case !(_, sum) =>
          sum.capitals

        case `⟦⟧`(_, _, sum, _) =>
          sum.capitals

        case `{}`(id, _, false) => Set(Symbol(id))

        case _ => Names()


    def rename(id: => String, free: Names, definition: Boolean = false)
              (using bindings: Bindings)
              (using refresh: MutableList[(Symbol, λ)])
              (using bound: Names): T =

      def rebind(it: Symbol)
                (using bound: Names): λ =
        val υidυ = Symbol(it.name.replaceAll("_υ.*υ", "") + id)
        bindings.find { case (_, Shadow(`it`)) => true case _ => false } match
          case Some((_, occurrence)) if definition && occurrence.isBinding =>
            bindings += it -> Binder(occurrence)(υidυ)
          case Some((_, occurrence)) =>
            bindings += it -> Shadow(occurrence)(υidυ)
          case _ =>
            refresh.prepend(it -> λ(υidυ))
        bound += υidυ
        λ(υidυ)

      inline def rename[S <: AST](ast: S)(using Names): S = ast.rename(id, free, definition)

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅(_) => ast

        case +(it*) =>
          `+`(it.map(rename(_))*)

        case ∥(it*) =>
          ∥(it.map(rename(_))*)

        case `.`(end, _it*) =>
          val n = refresh.size
          given Names = Names(bound)
          val it = _it.map {
            case ν(_names*) =>
              val names = _names.map(Symbol(_)).map(rebind(_))
              ν(names.map(_.asSymbol.name)*)
            case it @ τ(given Option[Code]) =>
              it.copy(code = recoded(free))
            case it @ π(λ(ch: Symbol), λ(par: Symbol), true, given Option[Code]) =>
              it.copy(channel = renamed(ch), name = rebind(par), code = recoded(free))
            case it @ π(λ(ch: Symbol), λ(arg: Symbol), false, given Option[Code]) =>
              it.copy(channel = renamed(ch), name = renamed(arg), code = recoded(free))
            case it @ π(λ(ch: Symbol), _, false, given Option[Code]) =>
              it.copy(channel = renamed(ch), code = recoded(free))
            case it => it
          }
          val end2 = rename(end)
          refresh.dropInPlace(refresh.size - n)
          `.`(end2, it*)

        case ?:(((λ(lhs: Symbol), λ(rhs: Symbol)), m), t, f) =>
          ?:(((renamed(lhs), renamed(rhs)), m), rename(t), f.map(rename(_)))

        case ?:(((λ(lhs: Symbol), rhs), m), t, f) =>
          ?:(((renamed(lhs), rhs), m), rename(t), f.map(rename(_)))

        case ?:(((lhs, λ(rhs: Symbol)), m), t, f) =>
          ?:(((lhs, renamed(rhs)), m), rename(t), f.map(rename(_)))

        case ?:(cond, t, f) =>
          ?:(cond, rename(t), f.map(rename(_)))

        case !(Some(it @ τ(given Option[Code])), sum) =>
          `!`(Some(it.copy(code = recoded(free))), rename(sum))

        case !(Some(it @ π(λ(ch: Symbol), λ(par: Symbol), true, given Option[Code])), sum) =>
          val n = refresh.size
          given Names = Names(bound)
          val π = it.copy(channel = renamed(ch), name = rebind(par), code = recoded(free))
          val sum2 = rename(sum)
          refresh.dropInPlace(refresh.size - n)
          `!`(Some(π), sum2)

        case !(Some(it @ π(λ(ch: Symbol), λ(arg: Symbol), false, given Option[Code])), sum) =>
          val π = it.copy(channel = renamed(ch), name = renamed(arg), code = recoded(free))
          `!`(Some(π), rename(sum))

        case !(Some(it @ π(λ(ch: Symbol), _, false, given Option[Code])), sum) =>
          val π = it.copy(channel = renamed(ch), code = recoded(free))
          `!`(Some(π), rename(sum))

        case it @ !(_, sum) =>
          it.copy(sum = rename(sum))

        case it @ `⟦⟧`(_, variables, sum, assign) =>
          val n = refresh.size
          val assign2 = assign
            .map { (variable, pointer) =>
              val υidυ = Symbol(variable.name.replaceAll("_υ.*υ", "") + id)
              refresh.prepend(variable -> λ(υidυ))
              υidυ -> renamed(pointer).asSymbol
            }
          var variables2 = variables
            .drop(assign.size)
            .map { it =>
              val υidυ = Symbol(it.name.replaceAll("_υ.*υ", "") + id)
              refresh.prepend(it -> λ(υidυ))
              υidυ
            }
          variables2 = assign2.map(_._1) ++ variables2
          val sum2 = rename(sum)
          refresh.dropInPlace(refresh.size - n)
          it.copy(variables = variables2, sum = sum2, assign = assign2)

        case `{}`(id, pointers, agent, params*) =>
          val pointers2 = pointers.map(renamed(_).asSymbol)
          val params2 = params
            .map {
              case λ(it: Symbol) => renamed(it)
              case it => it
            }

          `{}`(id, pointers2, agent, params2*)

        case `(*)`(id, qual, params*) =>
          val params2 = params
            .map {
              case λ(it: Symbol) => renamed(it)
              case it => it
            }

          `(*)`(id, qual, params2*)
