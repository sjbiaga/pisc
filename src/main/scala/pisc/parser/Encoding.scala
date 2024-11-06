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

import scala.collection.mutable.{ ListBuffer => MutableList, LinkedHashSet => Set }

import scala.meta.Term

import scala.util.parsing.combinator._

import Expression.Code
import PolyadicPi._
import Calculus._
import Encoding._


abstract class Encoding extends Calculus:

  def definition: Parser[Definition] =
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
            val `macro` = Macro(parameters.toList, constants, variables, given_Names2, sum)
            Definition(_code, term, Some(`macro`), Nil, constants, variables, sum)
        }
    }

  def instantiation(using binding2: Names2): Parser[(`⟦⟧`, Names)] =
    given Names2 = Names2(binding2)
    regexMatch("""⟦(\d*)""".r) >> { m =>
      nest(true)
      val grp1 = m.group(1)
      val code = if grp1.isEmpty
                 then
                   val def1 = defn.filter { (_, it) => it.size == 1 && it.head.term.isEmpty }
                   if def1.size == 1
                   then def1.head._2.head.code
                   else -1
                 else
                   grp1.toInt
      defn.get(code) match {
        case Some(it) => it
        case _ if grp1.nonEmpty => throw NoDefinitionException(grp1.toInt)
        case _ => defn.values.reduce(_ ++ _).filterNot(_.term.isEmpty)
      } match
        case (definition @ Definition(_, None, _, _, _, _, _)) :: Nil =>
          (choice <~ s"$grp1⟧") ~ opt( pointers ) ^^ {
            case (sum, free) ~ ps =>
              (`⟦⟧`(definition, sum) -> free) -> ps
          }
        case it =>
          (instance(it, s"$grp1⟧") <~ s"$grp1⟧") ~ opt( pointers ) ^^ {
            case exp ~ ps =>
              exp -> ps
          }
    } ^^ {
      case ((exp @ `⟦⟧`(Definition(_, _, _, _, constants, variables, _), _, _), free), _pointers) =>
        nest(false)
        given MutableList[(Symbol, λ)]()
        val pointers = _pointers.map(_._1.map(renamed(_).asSymbol)).getOrElse(Nil)
        val _assign = variables zip pointers
        val assign = if _assign.isEmpty then None else Some(_assign)
        Expression.renaming = Some((given_MutableList_Symbol_λ, given_Names2))
        given Names = Names()
        val exp2 = exp.copy(assign = assign).rename(free)
        binding2 ++= given_Names2.filter(_._2.isBinding < 0)
        exp2 -> (free ++ constants)
    }

  def instance(defs: List[Definition], end: String)(using Names2): Parser[(`⟦⟧`, Names)]

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


object Encoding:

  import scala.annotation.tailrec

  case class Macro(parameters: List[Symbol],
                   constants: Names,
                   variables: Names,
                   binding2: Names2,
                   sum: +):
    def apply(code: Int, term: Term): Definition =
      given refresh: MutableList[(Symbol, λ)] = MutableList()
      val variables2 = variables
        .map { it =>
          val υidυ = Symbol(it.name.replaceAll("_υ.*υ", "") + id)
          refresh.prepend(it -> λ(υidυ))
          υidυ
        }
      given Names2 = Names2(this.binding2)
      Expression.renaming = Some((refresh, given_Names2))
      given Names = Names()
      val sum2 = sum.rename(Set.empty, definition = true)
      val binding2 = given_Names2.filter(_._2.isBinding < 0)
      val shadows = (
        parameters.map(_ -> None).toMap
        ++
        binding2.collect {
          case (it, Binder(υidυ)) => (
            binding2.find { case (_, Shadow(`it`)) => true case _ => false } match
              case Some((it, _)) => it
              case _ => it
          ) -> Some(υidυ)
        }
      ) .toList
        .sortBy { (it, _) => parameters.indexOf(it) }
        .map(_._2)
      Definition(code, Some(term), None, shadows, constants, variables2, sum2)

  case class Definition(code: Int,
                        term: Option[Term],
                        `macro`: Option[Macro],
                        shadows: List[Option[Symbol]],
                        constants: Names,
                        variables: Names,
                        sum: +):
    override def toString: String =
      ( term match
          case Some(term) => if code == 0 then s"⟦ $term ⟧" else s"⟦$code $term $code⟧"
          case _ => if code == 0 then s"⟦ ⟧" else s"⟦$code $code⟧"
      ) + (if constants.isEmpty then "" else constants.map(_.name).mkString("(", ", ", ")"))
        + (if variables.isEmpty then "" else variables.map(_.name).mkString("{", ", ", "}"))
        + " = " + sum


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

  case class NoBindingParsingException(name: String)
      extends BindingParsingException(s"No binding for $name")


  // functions

  @tailrec
  def shadow(it: Symbol)
            (using binding2: Names2): Option[Occurrence] =
    if binding2.contains(it)
    then
      binding2.find { case (`it`, Shadow(_)) => true case _ => false } match
        case Some((_, occurrence @ Shadow(`it`))) => Some(occurrence)
        case Some((_, Shadow(it))) => shadow(it)
        case _ => None
    else
      binding2.find { case (_, Shadow(`it`)) => true case _ => false } match
        case Some((_, occurrence)) => Some(occurrence)
        case _ => None

  def renamed(it: Symbol)
             (using refresh: MutableList[(Symbol, λ)])
             (using binding2: Names2): λ =
    refresh.find(_._1 == it) match
      case Some((_, r)) => r
      case _ =>
        binding2.find { case (`it`, Binder(_)) => true case _ => false } match
          case Some((_, Binder(υidυ))) => λ(υidυ)
          case _ => shadow(it) match
            case Some(Shadow(it)) => λ(it)
            case _ if binding2.contains(it) => λ(it)
            case _ => throw NoBindingParsingException(it.name)

  def recoded(free: Names)
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

  private def _removed(it: Symbol)
                      (using binding2: Names2): Option[Either[Symbol, Symbol]] =
    var name: Option[Either[Symbol, Symbol]] = None
    binding2.filterInPlace {
      case (`it`, Shadow(`it`)) => name = Some(Left(it)); false
      case (it, Shadow(`it`)) => name = Some(Right(it)); false
      case _ => true
    }
    name.map(_.flatMap(_removed(_).orElse(name).get))

  inline def removed(it: Symbol)
                    (using binding2: Names2): Symbol =
    _removed(it).get.fold(identity, identity)


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

        case `⟦⟧`(_, sum, _) =>
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
            binding2 += removed(it) -> Shadow(occurrence, it)
            binding2 += it -> Binder(occurrence, υidυ)
          case Some((_, occurrence)) =>
            binding2 += it -> Shadow(occurrence, υidυ)
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
            case it @ τ(given Option[Code]) =>
              it.copy(code = recoded(free))
            case π(λ(ch: Symbol), true, code, _names*) =>
              given Option[Code] = code
              val ch2 = renamed(ch)
              val names = _names.map(_.asSymbol).map(rebind(_))
              π(ch2, true, recoded(free), names*)
            case π(λ(ch: Symbol), false, code, _names*) =>
              given Option[Code] = code
              val ch2 = renamed(ch)
              val names = _names.map {
                case λ(arg: Symbol) => renamed(arg)
                case it => it
              }
              π(ch2, false, recoded(free), names*)
            case ν(_names*) =>
              val names = _names.map(Symbol(_)).map(rebind(_))
              ν(names.map(_.asSymbol.name)*)
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

        case !(Some(π(λ(ch: Symbol), true, code, _names*)), sum) =>
          given Option[Code] = code
          val n = refresh.size
          given Names = Names(binding)
          val ch2 = renamed(ch)
          val names = _names.map(_.asSymbol).map(rebind(_))
          val it = π(ch2, true, recoded(free), names*)
          val sum2 = rename(sum)
          refresh.dropInPlace(refresh.size - n)
          `!`(Some(it), rename(sum))

        case !(Some(π(λ(ch: Symbol), false, code, _names*)), sum) =>
          given Option[Code] = code
          val names = _names.map {
            case λ(arg: Symbol) => renamed(arg)
            case it => it
          }
          `!`(Some(π(renamed(ch), false, recoded(free), names*)), rename(sum))

        case it @ !(_, sum) =>
          it.copy(sum = rename(sum))

        case it @ `⟦⟧`(definition @ Definition(_, _, _, _, _, variables, _), sum, assign) =>
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
          val definition2 = definition.copy(variables = variables2)
          val sum2 = rename(sum)
          refresh.dropInPlace(refresh.size - n)
          it.copy(definition = definition2, sum = sum2, assign = assign2)

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

  private[parser] var _id: scala.collection.mutable.Seq[Char] = null
  private[parser] var _ix: Int = -1
  /**
    * @return unique identifier of the form "_υ[0-9a-zA-Z]+υ"
    */
  def id: String =
    var reset = false
    while _ix >= 0 && _id(_ix) == 'Z'
    do
      _id(_ix) = '0'
      _ix -= 1
      reset = true
    if _ix < 0
    then
      _id :+= '1'
    else
      _id(_ix) match
        case 'z' =>
          _id(_ix) = 'A'
        case '9' =>
          _id(_ix) = 'a'
        case it =>
          _id(_ix) = (it + 1).toChar
    if reset then _ix = _id.size - 1
    "_υ" + _id.mkString + "υ"
