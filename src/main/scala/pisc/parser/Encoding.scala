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

package pisc
package parser

import scala.collection.mutable.{
  LinkedHashMap => Map,
  ListBuffer => MutableList,
  LinkedHashSet => Set
}

import scala.meta.Term

import scala.util.parsing.combinator._

import Expression.Code
import Pi._
import Calculus._
import Encoding._
import scala.util.parsing.combinator.pisc.parser.Expansion.replace


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
        val binding = _parameters ++ constants ++ variables
        given Names2 = Names2() ++
                       binding
                         .filterNot(_.name.charAt(0).isUpper)
                         .map { it => it -> Occurrence(None, (if parameters.contains(it) then pos_() else pos())) }
        choice ^^ {
          case (_sum, _free) =>
            val sum = _sum.flatten
            val free = _free ++ sum.capitals
            if (free &~ binding).nonEmpty
            then
              throw DefinitionFreeNamesException(_code, free &~ binding)
            if parameters.size == _parameters.size
            then
              eqtn :+= `(*)`("Self_" + _code, Nil, binding.map(λ(_)).toSeq*) -> sum
            val `macro` = Macro(parameters.toList, _parameters.size, constants, variables, given_Names2, sum)
            `macro` -> Definition(_code, term, constants, variables, sum)
        }
    }

  def instantiation(using binding2: Names2): Parser[(`⟦⟧`, Names)] =
    given Names2 = Names2(binding2)
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
        val pointers = _pointers.map(_._1.map(renamed(_).asSymbol)).getOrElse(Nil)
        val _assign = variables zip pointers
        val assign = if _assign.isEmpty then None else Some(_assign)
        Expression.renaming = Some((given_MutableList_Symbol_λ, given_Names2))
        Expression.replacing = None
        Expression.updating = None
        given Names = Names()
        val exp2 = exp.copy(assign = assign).rename(free)
        binding2 ++= purged
        exp2 -> (free ++ constants)
    }

  def instance(defs: List[Define], end: String)
              (using Names2): Parser[(`⟦⟧`, Names)]

  private def pointers: Parser[(List[Symbol], Names)] =
    "{"~>rep1sep(name, ",")<~"}" ^^ {
      case ps if !ps.forall(_._1.isSymbol) =>
        throw PointersParsingException(ps.filterNot(_._1.isSymbol).map(_._1)*)
      case ps =>
        ps.map(_._1.asSymbol) -> ps.map(_._2).reduce(_ ++ _)
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

  protected val _cache = Map[CacheKey, CacheValue]()


object Encoding:

  type Define = (Macro, Definition)

  type Fresh = (Definition, (Int, List[Option[Symbol]]))

  private type CacheKey = ((Seq[Long], (String, Either[String, String])), Int)

  private type CacheValue = (`⟦⟧` | +, Any, Names, Names2, Input)

  case class Macro(parameters: List[Symbol],
                   arity: Int,
                   constants: Names,
                   variables: Names,
                   binding2: Names2,
                   sum: +):
    def apply(code: Int, term: Term): Fresh =
      given refresh: MutableList[(Symbol, λ)] = MutableList()
      val variables2 = variables
        .map { it =>
          val υidυ = Symbol(it.name.replaceAll("_υ.*υ", "") + id)
          refresh.prepend(it -> λ(υidυ))
          υidυ
        }
      given Names2 = Names2(this.binding2)
      Expression.renaming = Some((refresh, given_Names2))
      Expression.replacing = None
      Expression.updating = None
      given Names = Names()
      val sum2 = sum.rename(Set.empty, definition = true)
      val shadows = (
        parameters.map(_ -> None).toMap
        ++
        purged.collect { case (it, Binder(υidυ)) => it -> Some(υidυ) }
      ) .toList
        .sortBy { (it, _) => parameters.indexOf(it) }
        .map(_._2)
      val `def` = Definition(code, Some(term), constants, variables2, sum2)
      `def` -> (arity - shadows.count(_.nonEmpty) -> shadows)

  case class Definition(code: Int,
                        term: Option[Term],
                        constants: Names,
                        variables: Names,
                        sum: +):
    inline def apply()(using substitution: Map[String, λ | AST]): `⟦⟧` =
      Expression.renaming = None
      Expression.updating = None
      Expression.replacing = Some(substitution)
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

  final case class Position(counter: Long, binding: Boolean)

  final case class Occurrence(shadow: Symbol | Option[Symbol], position: Position):
    val isBinding = if !position.binding then 0 else math.signum(position.counter)

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

  type Names2 = Map[Symbol, Occurrence]

  object Names2:
    def apply(): Names2 = Map()
    def apply(binding2: Names2): Names2 = Map.from(binding2)
    def apply(names: Names)
             (using Names2): Unit =
      names.foreach { it => this(it, if _code < 0 then None else Some(it), hardcoded = true) }
    def apply(name: Symbol, shadow: Option[Symbol], hardcoded: Boolean = false)
             (using binding2: Names2): Unit =
      binding2.get(name) match
        case Some(Occurrence(_, it @ Position(k, false))) if k < 0 =>
          binding2 += name -> Occurrence(shadow, it.copy(binding = true))
        case Some(Occurrence(_, Position(k, true))) if _code >= 0 && (!hardcoded || k < 0) =>
          throw UniquenessBindingParsingException(name, hardcoded)
        case Some(Occurrence(_, Position(_, false))) if _code >= 0 =>
          throw NonParameterBindingParsingException(name, hardcoded)
        case Some(Occurrence(_, Position(_, false))) =>
        case _ =>
          binding2 += name -> Occurrence(shadow, pos(true))


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

  abstract class BindingParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg
                                 + s" at nesting level #$_nest"
                                 + (if _code >= 0 then s" in the right hand side of encoding $_code" else ""), cause)

  case class NoBindingParsingException(name: String)
      extends BindingParsingException(s"No binding for $name")

  case class UniquenessBindingParsingException(name: Symbol, hardcoded: Boolean)
      extends BindingParsingException(s"""A binding name (${name.name}) does not correspond to a unique ${if hardcoded then "hardcoded" else "encoded"} binding occurrence, but is duplicated""")

  case class NonParameterBindingParsingException(name: Symbol, hardcoded: Boolean)
      extends BindingParsingException(s"""A binding name (${name.name}) in ${if hardcoded then "a hardcoded" else "an encoded"} binding occurrence does not correspond to a parameter""")


  // functions

  def renamed(it: Symbol)
             (using refresh: MutableList[(Symbol, λ)])
             (using binding2: Names2): λ =
    refresh.find(_._1 == it) match
      case Some((_, r)) => r
      case _ =>
        binding2.find { case (`it`, Binder(_) | Shadow(_)) => true case _ => false } match
          case Some((_, Binder(it))) => λ(it)
          case Some((_, Shadow(it))) => λ(it)
          case _ =>
            binding2.find { case (`it`, _) | (_, Shadow(`it`)) => true case _ => false } match
              case Some(_) => λ(it)
              case _ => throw NoBindingParsingException(it.name)

  private def recoded(free: Names)
                     (using code: Option[Code])
                     (using MutableList[(Symbol, λ)])
                     (using Names2)
                     (using binding: Names): Option[Code] =
    code.map { (_, orig) =>
      val term = Expression(orig)._1
      val (code2, names) = Expression.recode(term)
      free ++= names.filterNot(binding.contains(_))
      code2
    }

  private def purged(using binding2: Names2): Names2 =
    binding2.map {
      case (name, Shadow(it)) =>
        binding2.find { case (`it`, Binder(_) | Shadow(_)) => true case _ => false } match
          case Some((_, occurrence)) =>
            Some(name -> (it -> occurrence))
          case _ =>
            None
      case _ =>
        None
    }.foreach {
      case Some((name, (it, occurrence))) =>
        binding2 -= it
        binding2 += name -> occurrence
      case _ =>
    }
    binders

  inline def binders(using binding2: Names2): Names2 =
    binding2.filter(_._2.isBinding < 0)


  extension[T <: AST](ast: T)

    def capitals: Names =

      ast match

        case ∅ => Names()

        case +(it*) => it.map(_.capitals).reduce(_ ++ _)

        case ||(it*) => it.map(_.capitals).reduce(_ ++ _)

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


    def rename(free: Names, definition: Boolean = false)
              (using binding2: Names2)
              (using refresh: MutableList[(Symbol, λ)])
              (using binding: Names): T =

      def rebind(it: Symbol)
                (using binding: Names): λ =
        val υidυ = Symbol(it.name.replaceAll("_υ.*υ", "") + id)
        binding2.find { case (_, Shadow(`it`)) => true case _ => false } match
          case Some((_, occurrence)) if definition && occurrence.isBinding < 0 =>
            binding2 += it -> Binder(occurrence)(υidυ)
          case Some((_, occurrence)) =>
            binding2 += it -> Shadow(occurrence)(υidυ)
          case _ =>
            refresh.prepend(it -> λ(υidυ))
        binding += υidυ
        λ(υidυ)

      inline def rename[S <: AST](ast: S)(using Names): S = ast.rename(free, definition)

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅ => ∅

        case +(it*) =>
          `+`(it.map(rename(_))*)

        case ||(it*) =>
          ||(it.map(rename(_))*)

        case `.`(end, _it*) =>
          val n = refresh.size
          given Names = Names(binding)
          val it = _it.map {
            case ν(_names*) =>
              val names = _names.map(_ -> Symbol(_)).map(_ -> rebind(_))
              ν(names.map(_ -> _.asSymbol.name)*)
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
          given Names = Names(binding)
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
            .map(
              _.map { (variable, pointer) =>
                val υidυ = Symbol(variable.name.replaceAll("_υ.*υ", "") + id)
                refresh.prepend(variable -> λ(υidυ))
                υidυ -> renamed(pointer).asSymbol
              }
            )
          var variables2 = variables
            .drop(assign.map(_.size).getOrElse(0))
            .map { it =>
              val υidυ = Symbol(it.name.replaceAll("_υ.*υ", "") + id)
              refresh.prepend(it -> λ(υidυ))
              υidυ
            }
          variables2 = assign2.map(_.map(_._1)).getOrElse(Names()) ++ variables2
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

          `{}`(id, pointers = pointers2, agent, params2*)

        case `(*)`(id, qual, params*) =>
          val params2 = params
            .map {
              case λ(it: Symbol) => renamed(it)
              case it => it
            }

          `(*)`(id, qual, params2*)

  private[parser] var _id: helper.υidυ = null

  def id = _id()

  def copy: Any = _id.copy

  def paste(it: Any) = _id.paste(it)
