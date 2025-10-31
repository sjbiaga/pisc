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

import scala.collection.mutable.{
  LinkedHashMap => Map,
  ListBuffer => MutableList,
  LinkedHashSet => Set
}

import scala.meta.Term

import Expression.Code
import Ambient.{ AST => _, * }
import Calculus.*
import Encoding.*
import scala.util.parsing.combinator.masc.parser.Expansion.{ replace, Duplications, Substitution }


abstract class Encoding extends Calculus:

  def definition(using Duplications): Parser[Option[Define]] =
    template ~ opt( "("~>names<~")" ) ~ opt( pointers ) >> {
      case (term, _parameters) ~ _constants ~ _variables =>
        val constants = _constants.map(_.map(_._2).reduce(_ ++ _)).getOrElse(Names())
        val variables = _variables.map(_._2).getOrElse(Names())
        val parameters = _parameters.filterNot(_.charAt(0).isUpper)
        if (parameters & constants).nonEmpty
        || (variables & parameters).nonEmpty
        || (constants & variables).nonEmpty
        then
          throw DefinitionParametersException(_code)
        var bound = _parameters ++ constants ++ variables
        given Bindings = Bindings() ++
                         bound
                           .filterNot(_.charAt(0).isUpper)
                           .map { it => it -> (if parameters.contains(it) then pos_() else pos()) }
                           .map(_ -> Occurrence(None, _))
        if _dir.isDefined
        then
          Directive()
          Success(Option.empty[Define], _)
        else
          given Int = 1
          "="~> parallel ^^ {
            case (_par, _free) =>
              val par = _par.flatten
              val free = _free ++ par.capitals
              val (constantsʹ, variablesʹ, bindingsʹ, parʹ) =
                par match
                  case ∥(`.`(exp @ `⟦⟧`(Definition(_, _, constantsʹ, _, _), variablesʹ, _, _, assignmentʹ)))
                      if assignmentʹ.size < variablesʹ.size =>
                    val constantsʹʹ = constantsʹ &~ constants
                    val pointersʹ = variablesʹ.map(_.replaceAll("_υ.*υ", ""))
                    val variablesʹʹ = (pointersʹ &~ variables)
                                        .map { it => variablesʹ.find(_.startsWith(it)).get }
                    val pointersʹʹ = pointersʹ.drop(assignmentʹ.size)
                    if variablesʹʹ.size != pointersʹʹ.size
                    then
                      warn(throw EncodingAliasAbandonedException(_code))
                      (constants, variables, given_Bindings, par)
                    else
                      val assignmentʹʹ = assignmentʹ ++ (variablesʹʹ zip pointersʹʹ)
                      val boundʹ = constantsʹʹ ++ pointersʹʹ
                      bound ++= boundʹ
                      val bindingsʹ = boundʹ.map(_ -> Occurrence(None, pos()))
                      val expʹ: `⟦⟧` = exp.copy(assignment = assignmentʹʹ)
                      val parʹ: ∥ = ∥(`.`(expʹ))
                      (constants ++ constantsʹʹ, variables ++ pointersʹʹ, given_Bindings ++ bindingsʹ, parʹ)
                  case _ =>
                    (constants, variables, given_Bindings, par)
              if (free &~ bound).nonEmpty
              then
                throw DefinitionFreeNamesException(_code, free &~ bound)
              if parameters.size == _parameters.size
              then
                if !_exclude
                then
                  val bind: `(*)` = `(*)`("Self_" + _code, Nil, bound.toSeq*)
                  eqtn :+= bind -> parʹ
              Some {
                Macro(parameters.toList, _parameters.size, constantsʹ, variablesʹ, bindingsʹ, parʹ)
                ->
                Definition(_code, term, constants, variables, par)
              }
          }
    }

  def instantiation(using bindings: Bindings, duplications: Duplications, _sc: Int): Parser[(`⟦⟧`, Names)] =
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
          (parallel <~ s"$grp1⟧") ~ opt( pointers ) ^^ {
            case (par, free) ~ ps =>
              val xid = χ_id
              duplications += xid -> (false, Map())
              (`⟦⟧`(definition, definition.variables, par.flatten, xid) -> free) -> ps
          }
        case it =>
          (instance(it, s"$grp1⟧") <~ s"$grp1⟧") ~ opt( pointers ) ^^ {
            case exp ~ ps =>
              exp -> ps
          }
    } ^^ {
      case ((exp @ `⟦⟧`(Definition(_, _, constants, _, _), variables, _, _, _), free), _pointers) =>
        try
          given MutableList[(String, String)]()
          val pointers = _pointers.map(_._1.map(renamed(_))).getOrElse(Nil)
          val assignment = variables zip pointers
          given Names()
          val expʹ = exp.copy(assignment = assignment).rename()(id)(free)
          bindings ++= purged
          nest(false)
          expʹ -> (free ++ constants)
        catch
          case it: NoBPEx => throw NoBindingParsingException(_code, _nest, it.getMessage)
          case it => throw it
    }

  def instance(defs: List[Define], end: String)
              (using Bindings, Duplications, Int): Parser[(`⟦⟧`, Names)]

  def pointers: Parser[(List[String], Names)] =
    "{"~>names<~"}" ^^ { _.unzip match
      case (it, ns) => it -> ns.reduce(_ ++ _)
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

  private object Directive:

    private def canonical: String => String =
      case "werr" => "errors"
      case "dups" => "duplications"
      case it => it

    private def key: String => Boolean = canonical andThen {
      case "errors" | "duplications"
         | "exclude" | "include"
         | "paceunit" => true
      case _ => false
    }

    private def boolean: Boolean =
      _dir.get._2 match
        case it: String =>
          it.toLowerCase match
            case "0" | "off" | "false" | "no" | "n" => false
            case "1" | "on" | "true" | "yes" | "y" => true
            case _ => throw DirectiveValueParsingException(_dir.get, "a boolean")
        case _ => throw DirectiveValueParsingException(_dir.get, "a boolean")

    private def keys: Set[String] =
      _dir.get._2 match
        case it: String if key(it) => Set(canonical(it))
        case it: List[String] if it.forall(key) => Set.from(it.map(canonical))
        case _ => throw DirectiveValueParsingException(_dir.get, "a comma separated list of valid keys")

    def apply(): Unit =

      canonical(_dir.get._1.toLowerCase) match

        case "errors" =>
          _werr = boolean

        case "duplications" =>
          _dups = boolean

        case "exclude" =>
          _exclude = boolean

        case "include" =>
          _exclude = !boolean

        case "paceunit" =>
          _paceunit = _dir.get._2 match
            case it: String => it
            case _ => throw DirectiveValueParsingException(_dir.get, "a time unit")

        case "push" =>
          try
            if boolean
            then
              _dirs ::= Map("errors" -> _werr,
                            "duplications" -> _dups,
                            "exclude" -> _exclude,
                            "paceunit" -> _paceunit)
          catch _ =>
            _dirs ::= Map.from {
              keys.map {
                case it @ "errors" => it -> _werr
                case it @ "duplications" => it -> _dups
                case "exclude" | "include" => "exclude" -> _exclude
                case it @ "paceunit" => it -> _paceunit
              }
            }

        case "pop" =>
          if boolean
          then
            _dirs.head.foreach {
              case ("errors", it: Boolean) => _werr = it
              case ("duplications", it: Boolean) => _dups = it
              case ("exclude", it: Boolean) => _exclude = it
              case ("paceunit", it: String) => _paceunit = it
              case _ => ???
            }
            _dirs = _dirs.tail

          if _dirs.isEmpty
          then
            val dir = _dir
            _dir = Some("push" -> "1")
            this()
            _dir = dir

        case _ => throw DirectiveKeyParsingException(_dir.get)


object Encoding:

  type Define = (Macro, Definition)

  type Fresh = (Definition, (Int, List[Option[String]]))

  type CacheKey = ((Seq[Long], (String, Either[String, String])), Int)

  private type CacheValue = (∥ | `⟦⟧`, (Any, Any), Names, Bindings, Encoding#Input)

  case class Macro(parameters: List[String],
                   arity: Int,
                   constants: Names,
                   variables: Names,
                   bindings: Bindings,
                   par: ∥):
    def apply(code: Int, term: Term, dups: Boolean)
             (id: => String, χ_id: => String)
             (using Duplications): Fresh =
      given Bindings = Bindings(bindings)
      given MutableList[(String, String)]()
      val variablesʹ = variables
        .map { it =>
          val υidυ = it.replaceAll("_υ.*υ", "") + id
          given_Bindings(υidυ) = given_Bindings(it)
          given_Bindings -= it
          given_MutableList_String_String.prepend(it -> υidυ)
          υidυ
        }
      given Names()
      val parʹ = par.rename(expansion = true, dups)(id, χ_id)()
      val shadows = (
        parameters.map(_ -> None).toMap
        ++
        purged.collect { case (it, Binder(υidυ)) => it -> Some(υidυ) }
      ) .toList
        .sortBy { (it, _) => parameters.indexOf(it) }
        .map(_._2)
      Definition(code, Some(term), constants, variablesʹ, parʹ)
      ->
      (arity - shadows.count(_.nonEmpty) -> shadows)

  case class Definition(code: Int,
                        term: Option[Term],
                        constants: Names,
                        variables: Names,
                        par: ∥):
    def apply(_code: Int, nest: Int, dups: Boolean,
              duplicated: (Bindings, Duplications) ?=> String => Term => Unit)
             (id: => String)
             (using duplications: Duplications)
             (using Bindings, Substitution): `⟦⟧` =
      if dups
      then
        val ids = MutableList[String]()
        val idsʹ = MutableList.from {
          duplications.flatMap {
            case (xid, (true, _)) => Some(xid)
            case _ => None
          }
        }
        given (∥ | `⟦⟧` => ∥ | `⟦⟧`) = { ast =>
          lazy val count: AST => Unit =
            case ∅() =>
            case ∥(it*) => it.foreach(count)
            case `.`(end, _*) => count(end)
            case !(_, _, _, par) => count(par)
            case `[]`(_, par) => count(par)
            case `go.`(_, par) => count(par)
            case it: `⟦⟧` if ids.contains(it.xid) =>
              duplications += it.xid -> (true -> duplications(it.xid)._2)
              count(it.par)
            case it: `⟦⟧` if idsʹ.contains(it.xid) =>
              count(it.par)
            case it: `⟦⟧` =>
              ids += it.xid
              count(it.par)
            case _ =>
          count(ast)
          try
            given MutableList[(String, String)]()
            given Names()
            ast.rename(false, dups, duplicated)(id)()
          catch
            case it: NoBPEx => throw NoBindingParsingException(_code, nest, it.getMessage)
            case it => throw it
        }
        lazy val reset: AST => Unit =
          case ∅() =>
          case ∥(it*) => it.foreach(reset)
          case `.`(end, _*) => reset(end)
          case !(_, _, _, par) => reset(par)
          case `[]`(_, par) => reset(par)
          case `go.`(_, par) => reset(par)
          case it: `⟦⟧` if ids.contains(it.xid) =>
            duplications += it.xid -> (false -> duplications(it.xid)._2)
            reset(it.par)
          case it: `⟦⟧` => reset(it.par)
          case _ =>
        val exp: `⟦⟧` = `⟦⟧`(this, variables, par.replace.flatten)
        reset(exp.par)
        exp
      else
        given (∥ | `⟦⟧` => ∥ | `⟦⟧`) = { ast =>
          try
            given MutableList[(String, String)]()
            given Names()
            ast.rename(false, dups, duplicated)(id)()
          catch
            case it: NoBPEx => throw NoBindingParsingException(_code, nest, it.getMessage)
            case it => throw it
        }
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

  final case class Position(counter: Long, binds: Boolean)

  final case class Occurrence(shadow: String | Option[String], position: Position):
    val isBinding = position.binds && position.counter < 0

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

  type Bindings = Map[String, Occurrence]

  object Bindings:
    def apply(): Bindings = Map()
    def apply(bindings: Bindings): Bindings = Map.from(bindings)


  // exceptions

  import Expression.ParsingException

  case class NoDefinitionException(code: Int)
      extends ParsingException(s"No definition for encoding $code")

  case class DefinitionParametersException(code: Int)
      extends EquationParsingException(s"The parameters, constants, and variables must all be different in the left hand side of encoding $code")

  case class DefinitionFreeNamesException(code: Int, free: Names)
      extends EquationParsingException(s"""The free names (${free.mkString(", ")}) in the right hand side are not formal parameters of the left hand side of encoding $code""")

  abstract sealed class BindingParsingException(code: Int, nest: Int, msg: String, cause: Throwable = null)
      extends ParsingException(msg
                                 + s" at nesting level #$nest"
                                 + (if code >= 0 then s" in the right hand side of encoding $code" else ""), cause)

  case class NoBindingParsingException(code: Int, nest: Int, name: String)
      extends BindingParsingException(code, nest, s"No binding for $name")

  final private class NoBPEx(name: String) extends Throwable(name)

  case class UniquenessBindingParsingException(code: Int, nest: Int, name: String, hardcoded: Boolean, how: String)
      extends BindingParsingException(code, nest, s"""A binding name ($name) does not correspond to a unique ${if hardcoded then "hardcoded" else "encoded"} binding occurrence, being $how""")

  case class NonParameterBindingParsingException(code: Int, nest: Int, name: String, hardcoded: Boolean)
      extends BindingParsingException(code, nest, s"""A binding name ($name) in ${if hardcoded then "a hardcoded" else "an encoded"} binding occurrence does not correspond to a parameter""")

  case class EncodingAliasAbandonedException(code: Int)
      extends ParsingException(s"An encoding alias was abandoned as the right hand side of encoding $code")

  abstract sealed class DirectiveParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  private object DirectiveParsingException:
    def apply(dir: (String, String | List[String])): String =
      dir._2 match
        case it: String => s"⟦ ${dir._1} = $it ⟧"
        case it: List[String] => s"""⟦ ${dir._1} = ${it.mkString("(", ",", ")")} ⟧"""

  case class DirectiveKeyParsingException(dir: (String, String | List[String]))
      extends DirectiveParsingException(s"The key in the directive ${DirectiveParsingException(dir)} is not valid")

  case class DirectiveValueParsingException(dir: (String, String | List[String]), `type`: String)
      extends DirectiveParsingException(s"The value in the directive ${DirectiveParsingException(dir)} is not ${`type`}")


  // functions

  def renamed(it: String)
             (using refresh: MutableList[(String, String)])
             (using bindings: Bindings): String =
    refresh.find(_._1 == it) match
      case Some((_, r)) => r
      case _ =>
        bindings.find { case (`it`, Binder(_) | Shadow(_)) => true case _ => false } match
          case Some((_, Binder(it))) => it
          case Some((_, Shadow(it))) => it
          case _ =>
            bindings.find { case (`it`, _) | (_, Shadow(`it`)) => true case _ => false } match
              case Some(_) => it
              case _ => throw NoBPEx(it)

  def recoded(free: Names)
             (using code: Option[Code])
             (using MutableList[(String, String)])
             (using Bindings)
             (using bound: Names): Option[Code] =
    code.map { (_, orig) =>
      val term = Expression(orig)._1
      val (codeʹ, names) = Expression.recode(term)
      free ++= names.filterNot(bound.contains(_))
      codeʹ
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
    binders

  inline def binders(using bindings: Bindings): Bindings =
    bindings.filter(_._2.isBinding)


  extension [T <: AST](ast: T)

    def capitals: Names =

      ast match

        case ∅() => Names()

        case ∥(it*) => it.map(_.capitals).reduce(_ ++ _)

        case `.`(end, _*) =>
          end.capitals

        case !(_, _, _, par) =>
          par.capitals

        case `[]`(_, par) =>
          par.capitals

        case `go.`(_, par) =>
          par.capitals

        case `⟦⟧`(_, _, par, _, _) =>
          par.capitals

        case `{}`(identifier, _, false) => Set(identifier)

        case _ => Names()


    def rename(expansion: Boolean = false, dups: Boolean = false,
               duplicated: (Bindings, Duplications) ?=> String => Term => Unit = { (_, _) ?=> { _ => { _ => } } })
              (id: => String, χ_id: => String = null)
              (free: Names = Names())
              (using bindings: Bindings)
              (using duplications: Duplications)
              (using refresh: MutableList[(String, String)])
              (using bound: Names): T =

      def rebind(it: String)
                (using bound: Names): String =
        val υidυ = it.replaceAll("_υ.*υ", "") + id
        bindings.find { case (_, Shadow(`it`)) => true case _ => false } match
          case Some((_, occurrence)) if expansion && occurrence.isBinding =>
            bindings += it -> Binder(occurrence)(υidυ)
          case Some((_, occurrence)) =>
            bindings += it -> Shadow(occurrence)(υidυ)
          case _ =>
            refresh.prepend(it -> υidυ)
        bound += υidυ
        υidυ

      inline def rename[S <: AST](ast: S)(using Names): S =
        ast.rename(expansion, dups, duplicated)(id, χ_id)(free)

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case ∥(it*) =>
          ∥(it.map(rename(_))*)

        case `.`(end, _it*) =>
          val n = refresh.size
          given Names = Names(bound)
          val it = _it.map {
            case ν(names*) =>
              ν(names.map(rebind(_))*)
            case it @ τ(given Option[Code]) =>
              it.copy(code = recoded(free))
            case `..`(_path*) =>
              val path = _path.map {
                case Λ(name) => Λ(renamed(name))
                case ζ(op, amb) => ζ(op, renamed(amb))
                case it => it
              }
              `..`(path*)
            case it @ `()`(name, given Option[Code]) =>
              it.copy(name = rebind(name), code = recoded(free))
          }
          val endʹ = rename(end)
          refresh.dropInPlace(refresh.size - n)
          `.`(endʹ, it*)

        case <>(given Option[Code], _path*) =>
          val path = _path.map {
            case Λ(name) => Λ(renamed(name))
            case ζ(op, amb) => ζ(op, renamed(amb))
            case it => it
          }
          <>(recoded(free), path*)

        case !(parallelism, pace, Some(name), par) =>
          val n = refresh.size
          given Names = Names(bound)
          val nameʹ = rebind(name)
          val parʹ = rename(par)
          refresh.dropInPlace(refresh.size - n)
          `!`(parallelism, pace, Some(nameʹ), parʹ)

        case it @ !(_, _, _, par) =>
          it.copy(par = rename(par))

        case `[]`(amb, par) =>
          `[]`(renamed(amb), rename(par))

        case `go.`(amb, par) =>
          `go.`(renamed(amb), rename(par))

        case it @ `⟦⟧`(Definition(_, term, _, _, _), variables, par, xid, assignment) =>
          if dups then term.foreach(duplicated(xid))
          val n = refresh.size
          val assignmentʹ = assignment
                              .map { (it, pt) =>
                                val υidυ = it.replaceAll("_υ.*υ", "") + id
                                refresh.prepend(it -> υidυ)
                                υidυ -> renamed(pt)
                              }
          val variablesʹ = assignmentʹ.map(_._1)
                        ++ variables
                             .drop(assignment.size)
                             .map { it =>
                               val υidυ = it.replaceAll("_υ.*υ", "") + id
                               refresh.prepend(it -> υidυ)
                               υidυ
                             }
          val parʹ = rename(par)
          refresh.dropInPlace(refresh.size - n)
          val xidʹ =
            if dups && expansion && term.isDefined
            then
              val υidυ = χ_id
              duplications += υidυ -> (false, Map())
              duplications(υidυ)._2.addAll(duplications(xid)._2)
              υidυ
            else
              xid
          it.copy(variables = variablesʹ, par = parʹ, xid = xidʹ, assignment = assignmentʹ)

        case `{}`(identifier, pointers, agent, params*) =>
          val pointersʹ = pointers.map(renamed(_))
          val paramsʹ = params.map(renamed(_))

          `{}`(identifier, pointersʹ, agent, paramsʹ*)

        case `(*)`(identifier, qual, params*) =>
          val paramsʹ = params.map(renamed(_))

          `(*)`(identifier, qual, paramsʹ*)
