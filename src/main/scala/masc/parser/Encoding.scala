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

import scala.collection.mutable.{
  LinkedHashMap => Map,
  ListBuffer => MutableList,
  LinkedHashSet => Set
}

import scala.meta.Term

import Expression.Code
import Ambient.{ AST => _, _ }
import Calculus._
import Encoding._
import scala.util.parsing.combinator.masc.parser.Expansion.replace


abstract class Encoding extends Calculus:

  def definition: Parser[Define] =
    template ~ opt( "("~>rep1sep(name, ",")<~")" ) ~ opt( pointers ) <~"=" >> {
      case (term, _parameters) ~ _constants ~ _variables =>
        val constants = _constants.map(_.map(_._2).reduce(_ ++ _)).getOrElse(Names())
        val variables = _variables.map(_._2).getOrElse(Names())
        val parameters = _parameters.filterNot(_.charAt(0).isUpper)
        if (parameters & constants).nonEmpty
        || (variables & parameters).nonEmpty
        || (constants & variables).nonEmpty
        then
          throw DefinitionParametersException(_code)
        val binding = _parameters ++ constants ++ variables
        given Names2 = Names2() ++
                       binding
                         .filterNot(_.charAt(0).isUpper)
                         .map { it => it -> Occurrence(None, (if parameters.contains(it) then pos_() else pos())) }
        parallel ^^ {
          case (_par, _free) =>
            val par = _par.flatten
            val free = _free ++ par.capitals
            if (free &~ binding).nonEmpty
            then
              throw DefinitionFreeNamesException(_code, free &~ binding)
            if parameters.size == _parameters.size
            then
              eqtn :+= `(*)`("Self_" + _code, Nil, binding.toSeq*) -> par
            Macro(parameters.toList, _parameters.size, constants, variables, given_Names2, par)
            ->
            Definition(_code, term, constants, variables, par)
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
          (parallel <~ s"$grp1⟧") ~ opt( pointers ) ^^ {
            case (par, free) ~ ps =>
              (`⟦⟧`(definition, definition.variables, par.flatten) -> free) -> ps
          }
        case it =>
          (instance(it, s"$grp1⟧") <~ s"$grp1⟧") ~ opt( pointers ) ^^ {
            case exp ~ ps =>
              exp -> ps
          }
    } ^^ {
      case ((exp @ `⟦⟧`(Definition(_, _, constants, _, _), variables, _, _), free), _pointers) =>
        nest(false)
        given MutableList[(String, String)]()
        val pointers = _pointers.map(_._1.map(renamed(_))).getOrElse(Nil)
        val _assign = variables zip pointers
        val assign = if _assign.isEmpty then None else Some(_assign)
        given Names()
        val exp2 =
          try
            exp.copy(assign = assign).rename(id, free)
          catch
            case it: NoBPEx => throw NoBindingParsingException(_code, _nest, it.getMessage)
            case it => throw it
        binding2 ++= purged
        exp2 -> (free ++ constants)
    }

  def instance(defs: List[Define], end: String)
              (using Names2): Parser[(`⟦⟧`, Names)]

  def pointers: Parser[(List[String], Names)] =
    "{"~>rep1sep(name, ",")<~"}" ^^ { ps =>
        ps.map(_._1) -> ps.map(_._2).reduce(_ ++ _)
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

  type Fresh = (Definition, (Int, List[Option[String]]))

  private type CacheKey = ((Seq[Long], (String, Either[String, String])), Int)

  private type CacheValue = (`⟦⟧` | ∥, Any, Names, Names2, Encoding#Input)

  case class Macro(parameters: List[String],
                   arity: Int,
                   constants: Names,
                   variables: Names,
                   binding2: Names2,
                   par: ∥):
    def apply(code: Int, id: => String, _code: Int, _nest: Int, term: Term): Fresh =
      given refresh: MutableList[(String, String)] = MutableList()
      val variables2 = variables
        .map { it =>
          val υidυ = it.replaceAll("_υ.*υ", "") + id
          refresh.prepend(it -> υidυ)
          υidυ
        }
      given Names2 = Names2(binding2)
      given Names()
      val par2 = par.rename(id, Set.empty, definition = true)
      val shadows = (
        parameters.map(_ -> None).toMap
        ++
        purged.collect { case (it, Binder(υidυ)) => it -> Some(υidυ) }
      ) .toList
        .sortBy { (it, _) => parameters.indexOf(it) }
        .map(_._2)
      Definition(code, Some(term), constants, variables2, par2)
      ->
      (arity - shadows.count(_.nonEmpty) -> shadows)

  case class Definition(code: Int,
                        term: Option[Term],
                        constants: Names,
                        variables: Names,
                        par: ∥):
    inline def apply()(using Map[String, String | AST]): `⟦⟧` =
      `⟦⟧`(this, variables, par.replace.flatten)

    override def toString: String = Definition(code, term)
      + (if constants.isEmpty then "" else constants.mkString("(", ", ", ")"))
      + (if variables.isEmpty then "" else variables.mkString("{", ", ", "}"))
      + " = " + par

  object Definition:

    def apply(code: Int, term: Option[Term]): String =
      term match
        case Some(term) => if code == 0 then s"⟦ $term ⟧" else s"⟦$code $term $code⟧"
        case _ => if code == 0 then s"⟦ ⟧" else s"⟦$code $code⟧"

  final case class Position(counter: Long, binding: Boolean)

  final case class Occurrence(shadow: String | Option[String], position: Position):
    val isBinding = if !position.binding then 0 else math.signum(position.counter)

  object Binder:
    def apply(self: Occurrence)(υidυ: String) = Occurrence(υidυ, self.position)
    def unapply(self: Occurrence): Option[String] =
      self.shadow match
        case it: String => Some(it)
        case _ => None

  object Shadow:
    def apply(self: Occurrence)(υidυ: String) = self.copy(shadow = Some(υidυ))
    def unapply(self: Occurrence): Option[String] =
      self.shadow match
        case it @ Some(_) => it
        case _ => None

  type Names2 = Map[String, Occurrence]

  object Names2:
    def apply(): Names2 = Map()
    def apply(binding2: Names2): Names2 = Map.from(binding2)


  // exceptions

  import Expression.ParsingException

  case class NoDefinitionException(code: Int)
      extends ParsingException(s"No definition for encoding $code")

  case class DefinitionParametersException(code: Int)
      extends EquationParsingException(s"The parameters, constants, and variables must all be different in the left hand side of encoding $code")

  case class DefinitionFreeNamesException(code: Int, free: Names)
      extends EquationParsingException(s"""The free names (${free.mkString(", ")}) in the right hand side are not formal parameters of the left hand side of encoding $code""")

  abstract class BindingParsingException(code: Int, nest: Int, msg: String, cause: Throwable = null)
      extends ParsingException(msg
                                 + s" at nesting level #$nest"
                                 + (if code >= 0 then s" in the right hand side of encoding $code" else ""), cause)

  case class NoBindingParsingException(code: Int, nest: Int, name: String)
      extends BindingParsingException(code, nest, s"No binding for $name")

  private class NoBPEx(name: String) extends Throwable(name)

  case class UniquenessBindingParsingException(code: Int, nest: Int, name: String, hardcoded: Boolean)
      extends BindingParsingException(code, nest, s"""A binding name ($name) does not correspond to a unique ${if hardcoded then "hardcoded" else "encoded"} binding occurrence, but is duplicated""")

  case class NonParameterBindingParsingException(code: Int, nest: Int, name: String, hardcoded: Boolean)
      extends BindingParsingException(code, nest, s"""A binding name ($name) in ${if hardcoded then "a hardcoded" else "an encoded"} binding occurrence does not correspond to a parameter""")


  // functions

  def renamed(it: String)
             (using refresh: MutableList[(String, String)])
             (using binding2: Names2): String =
    refresh.find(_._1 == it) match
      case Some((_, r)) => r
      case _ =>
        binding2.find { case (`it`, Binder(_) | Shadow(_)) => true case _ => false } match
          case Some((_, Binder(it))) => it
          case Some((_, Shadow(it))) => it
          case _ =>
            binding2.find { case (`it`, _) | (_, Shadow(`it`)) => true case _ => false } match
              case Some(_) => it
              case _ => throw NoBPEx(it)

  def recoded(free: Names)
             (using code: Option[Code])
             (using MutableList[(String, String)])
             (using Names2)
             (using binding: Names): Option[Code] =
    code.map { (_, orig) =>
      val term = Expression(orig)._1
      val (code2, names) = Expression.recode(term)
      free ++= names.filterNot(binding.contains(_))
      code2
    }

  def purged(using binding2: Names2): Names2 =
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

        case ∥(it*) => it.map(_.capitals).reduce(_ ++ _)

        case `.`(end, _*) =>
          end.capitals

        case !(_, par) =>
          par.capitals

        case `[]`(_, par) =>
          par.capitals

        case `go.`(_, par) =>
          par.capitals

        case `⟦⟧`(_, _, par, _) =>
          par.capitals

        case `{}`(id, _, false) => Set(id)

        case _ => Names()


    def rename(id: => String, free: Names, definition: Boolean = false)
              (using binding2: Names2)
              (using refresh: MutableList[(String, String)])
              (using binding: Names): T =

      def rebind(it: String)
                (using binding: Names): String =
        val υidυ = it.replaceAll("_υ.*υ", "") + id
        binding2.find { case (_, Shadow(`it`)) => true case _ => false } match
          case Some((_, occurrence)) if definition && occurrence.isBinding < 0 =>
            binding2 += it -> Binder(occurrence)(υidυ)
          case Some((_, occurrence)) =>
            binding2 += it -> Shadow(occurrence)(υidυ)
          case _ =>
            refresh.prepend(it -> υidυ)
        binding += υidυ
        υidυ

      inline def rename[S <: AST](ast: S)(using Names): S = ast.rename(id, free, definition)

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅ => ∅

        case ∥(it*) =>
          ∥(it.map(rename(_))*)

        case `.`(end, _it*) =>
          val n = refresh.size
          given Names = Names(binding)
          val it = _it.map {
            case ν(names*) =>
              ν(names.map(rebind(_))*)
            case it @ τ(given Option[Code]) =>
              it.copy(code = recoded(free))
            case `,.`(_path*) =>
              val path = _path.map {
                case Λ(name) => Λ(renamed(name))
                case ζ(op, amb) => ζ(op, renamed(amb))
                case it => it
              }
              `,.`(path*)
            case it @ `()`(name, given Option[Code]) =>
              it.copy(name = rebind(name), code = recoded(free))
          }
          val end2 = rename(end)
          refresh.dropInPlace(refresh.size - n)
          `.`(end2, it*)

        case <>(given Option[Code], _path*) =>
          val path = _path.map {
            case Λ(name) => Λ(renamed(name))
            case ζ(op, amb) => ζ(op, renamed(amb))
            case it => it
          }
          <>(recoded(free), path*)

        case !(Some(name), par) =>
          val n = refresh.size
          given Names = Names(binding)
          val rep = `!`(Some(rebind(name)), rename(par))
          refresh.dropInPlace(refresh.size - n)
          rep

        case it @ !(_, par) =>
          it.copy(par = rename(par))

        case `[]`(amb, par) =>
          `[]`(renamed(amb), rename(par))

        case `go.`(amb, par) =>
          `go.`(renamed(amb), rename(par))

        case it @ `⟦⟧`(_, variables, par, assign) =>
          val n = refresh.size
          val assign2 = assign
            .map(
              _.map { (variable, pointer) =>
                val υidυ = variable.replaceAll("_υ.*υ", "") + id
                refresh.prepend(variable -> υidυ)
                υidυ -> renamed(pointer)
              }
            )
          var variables2 = variables
            .drop(assign.map(_.size).getOrElse(0))
            .map { it =>
              val υidυ = it.replaceAll("_υ.*υ", "") + id
              refresh.prepend(it -> υidυ)
              υidυ
            }
          variables2 = assign2.map(_.map(_._1)).getOrElse(Names()) ++ variables2
          val par2 = rename(par)
          refresh.dropInPlace(refresh.size - n)
          it.copy(variables = variables2, par = par2, assign = assign2)

        case `{}`(id, pointers, agent, params*) =>
          val pointers2 = pointers.map(renamed(_))
          val params2 = params.map(renamed(_))

          `{}`(id, pointers = pointers2, agent, params2*)

        case `(*)`(id, qual, params*) =>
          val params2 = params.map(renamed(_))

          `(*)`(id, qual, params2*)
