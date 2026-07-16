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

package basc
package parser

import scala.collection.mutable.{
  LinkedHashMap => Map,
  ListBuffer => MutableList,
  LinkedHashSet => Set
}

import scala.meta.Term

import helper.υidυ.rewrite

import Expression.Code
import BioAmbients.*
import Calculus.*
import Encoding.*
import scala.util.parsing.combinator.basc.parser.Expansion.Duplications


abstract class Encoding extends Calculus:

  def definition(using Duplications): Parser[Option[Define]] =
    template ~ opt( "("~>names<~")" ) ~ opt( pointers ) >> {
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
        if _dir.isDefined
        then
          Directive()
          Success(Option.empty[Define], _)
        else
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
                if !_exclude
                then
                  val bind: `(*)` = `(*)`("Self_" + _code, bound.map(λ(_)).toSeq*)
                  if _traces.isDefined
                  then
                    eqtn :+= bind -> sum.labelʹ(using bind.identifier -> _traces.get.getOrElse(""))
                  else
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

  private object Directive:

    private def canonical: String => String =
      case "werr" => "errors"
      case "dups" => "duplications"
      case it     => it

    private def key: String => Boolean = canonical andThen {
      case "echo"
         | "errors" | "duplications"
         | "exclude" | "include"
         | "paceunit"
         | "scaling"
         | "replication"
         | "typeclasses"
         | "parallelism"
         | "snapshot"
         | "traces" => true
      case _        => false
    }

    private implicit def ?[S, T](fun: S => T): S ?=> T = { it ?=> fun(it) }

    given `_String | List[String]_`: {} with

      extension (self: String | List[String])
                (using err: String => ((String, String | List[String])) ?=> Throwable = { msg => dir ?=> DirectiveValueParsingException(dir, msg) })
                (using key: () => String = () => _dir.get._1)
                (using dir: (() => String) ?=> (String, String | List[String]) = { key ?=> key() -> self })

        def boolean: Boolean =
          self match
            case it: String =>
              it.toLowerCase match
                case "0" | "off" | "false" | "no" | "n" => false
                case "1" | "on" | "true" | "yes" | "y"  => true
                case _                                  => throw err("a boolean")
            case _          => throw err("a boolean")

        def number: Int =
          self match
            case it: String =>
              try
                it.toInt
              catch
                case _: NumberFormatException =>
                  throw err("a number")
            case _          => throw err("a number")

        def file: Option[String] =
          self match
            case it: String if it.toLowerCase == "console" => None
            case _: String                                 => Some(self.string("<console> or a filename"))
            case _                                         => throw err("<console> or a filename")

        def string(`type`: String = "a string"): String =
          self match
            case it: String
                if (it.startsWith("\"") || it.startsWith("'"))
                && it.endsWith(s"${it.charAt(0)}") && it.length >= 2 =>
              it.substring(1, it.length-1)
            case _                                                   => throw err(`type`)

        def keys: Set[String] =
          self match
            case it: String if Directive.key(it)              => Set(canonical(it))
            case it: List[String] if it.forall(Directive.key) => Set.from(it.map(canonical))
            case _                                            => throw err("a comma separated list of valid keys")

    private def boolean: Boolean = _dir.get._2.boolean
    private def number: Int = _dir.get._2.number
    private def file: Option[String] = _dir.get._2.file
    private def string(`type`: String = "a string"): String = _dir.get._2.string(`type`)
    private def keys: Set[String] = _dir.get._2.keys

    private lazy val settings = Map("replication" -> "a <parallelism> number or a <linear> boolean setting")
    private def setting(using key: () => String = () => _dir.get._1) = settings(key().toLowerCase)

    def apply(): Unit =

      canonical(_dir.get._1.toLowerCase) match

        case "echo"         =>
          Console.println(_dir.get._2)

        case "errors"       =>
          _werr = boolean

        case "duplications" =>
          _dups = boolean

        case "exclude"      =>
          _exclude = boolean

        case "include"      =>
          _exclude = !boolean

        case "paceunit"     =>
          _paceunit = _dir.get._2 match
            case it: String => it
            case _          => throw DirectiveValueParsingException(_dir.get, "a time unit")

        case "scaling"      =>
          _scaling = boolean

        case "replication"  =>
          _replication = _dir.get._2 match
            case it: List[String] => it.map(_.toLowerCase) match
              case List("parallelism", it: String) =>
                (-1 max it.number(using { msg => DirectiveValueParsingException(_, setting, DirectiveSettingParsingException("parallelism", msg, it)) }), _replication._2)
              case List("linear", it: String)      =>
                (_replication._1, it.boolean(using { msg => DirectiveValueParsingException(_, setting, DirectiveSettingParsingException("linear", msg, it)) }))
              case _                               => throw DirectiveValueParsingException(_dir.get, setting)
            case _                => throw DirectiveValueParsingException(_dir.get, setting)

        case "typeclasses"  =>
          _typeclasses = _dir.get._2 match
            case it: String       => List(it)
            case it: List[String] => it

        case "parallelism"  =>
          _par = 1 max number.toInt

        case "snapshot" =>
          _snapshot = boolean

        case "traces"       =>
          try
            if boolean
            then
              _traces = Some(None)
            else
              _traces = None
          catch _ =>
             try
               _traces = Some(file)
             catch _ =>
               throw DirectiveValueParsingException(_dir.get, "a boolean, <console> or a filename")

        case "push"         =>
          try
            if boolean
            then
              _dirs ::= Map("echo"         -> (),
                            "errors"       -> _werr,
                            "duplications" -> _dups,
                            "exclude"      -> _exclude,
                            "paceunit"     -> _paceunit,
                            "scaling"      -> _scaling,
                            "replication"  -> _replication,
                            "typeclasses"  -> _typeclasses,
                            "parallelism"  -> _par,
                            "snapshot"     -> _snapshot,
                            "traces"       -> _traces)
          catch _ =>
            _dirs ::= Map.from {
              keys.map {
                case it @ "echo"           => it -> ()
                case it @ "errors"         => it -> _werr
                case it @ "duplications"   => it -> _dups
                case "exclude" | "include" => "exclude" -> _exclude
                case it @ "paceunit"       => it -> _paceunit
                case it @ "scaling"        => it -> _scaling
                case it @ "replication"    => it -> _replication
                case it @ "typeclasses"    => it -> _typeclasses
                case it @ "parallelism"    => it -> _par
                case it @ "snapshot"       => it -> _snapshot
                case it @ "traces"         => it -> _traces
              }
            }

        case "pop"          =>
          if boolean
          then
            _dirs.head.foreach {
              case ("echo", _)                            =>
              case ("errors", it: Boolean)                => _werr = it
              case ("duplications", it: Boolean)          => _dups = it
              case ("exclude", it: Boolean)               => _exclude = it
              case ("paceunit", it: String)               => _paceunit = it
              case ("scaling", it: Boolean)               => _scaling = it
              case ("replication", it: (Int, Boolean))    => _replication = it
              case ("typeclasses", it: List[String])      => _typeclasses = it
              case ("parallelism", it: Int)               => _par = it
              case ("snapshot", it: Boolean)              => _snapshot = it
              case ("traces", it: Option[Option[String]]) => _traces = it
              case _                                      => ???
            }
            _dirs = _dirs.tail

          if _dirs.isEmpty
          then
            val dir = _dir
            _dir = Some("push" -> "1")
            this()
            _dir = dir

        case _              => throw DirectiveKeyParsingException(_dir.get)


object Encoding:

  type Define = (Macro, Definition)

  type Fresh = (Definition, (Int, List[Option[Symbol]]))

  type CacheKey = ((Seq[Long], (String, Either[String, String])), Int)

  private type CacheValue = (+ | `⟦⟧`, (((Any, Any), Any), Any), Names, Bindings, Encoding#Input)

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

  abstract sealed class DirectiveParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  private object DirectiveParsingException:
    def apply(dir: (String, String | List[String])): String =
      dir._2 match
        case it: String => s"⟦ ${dir._1} = $it ⟧"
        case it: List[String] => s"""⟦ ${dir._1} = ${it.mkString("(", ",", ")")} ⟧"""

  case class DirectiveKeyParsingException(dir: (String, String | List[String]))
      extends DirectiveParsingException(s"The key in the directive ${DirectiveParsingException(dir)} is not valid")

  case class DirectiveValueParsingException(dir: (String, String | List[String]), `type`: String, cause: Throwable = null)
      extends DirectiveParsingException(s"The value in the directive ${DirectiveParsingException(dir)} is not ${`type`}", cause)

  case class DirectiveSettingParsingException(key: String, `type`: String, `val`: String)
      extends DirectiveParsingException(s"The <${key}> setting with value '${`val`}' is not ${`type`}")


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


  given Conversion[Symbol, λ] = λ(_)

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
            case it @ τ(_, given Option[Code]) =>
              it.copy(code = recoded)(it.id)
            case it @ π(_, λ(ch: Symbol), λ(params: List[`λ`]), Some(_), _, given Option[Code]) =>
              val paramsʹ = params.map {
                case par @ λ(Symbol("")) => par
                case λ(par: Symbol) => rebind(par)
              }
              it.copy(channel = renamed(ch), name = λ(paramsʹ), code = recoded)(it.id)
            case it @ π(_, λ(ch: Symbol), λ(par: Symbol), Some(_), _, given Option[Code]) =>
              it.copy(channel = renamed(ch), name = rebind(par), code = recoded)(it.id)
            case it @ π(_, λ(ch: Symbol), λ(arg: Symbol), None, _, given Option[Code]) =>
              it.copy(channel = renamed(ch), name = renamed(arg), code = recoded)(it.id)
            case it @ π(_, λ(ch: Symbol), _, None, _, given Option[Code]) =>
              it.copy(channel = renamed(ch), code = recoded)(it.id)
            case it @ ζ(_, name, _, _, given Option[Code]) =>
              it.copy(name = renamed(Symbol(name)).asSymbol.name, code = recoded)(it.id)
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

        case it @ !(_, _, Some(τ @ τ(_, given Option[Code])), _) =>
          it.copy(guard = Some(τ.copy(code = recoded)(τ.id)))

        case it @ !(_, _, Some(π @ π(_, λ(ch: Symbol), λ(par: Symbol), Some(_), _, given Option[Code])), sum) =>
          val n = refresh.size
          val πʹ = π.copy(channel = renamed(ch), name = rebind(par), code = recoded)(π.id)
          val sumʹ = rename(sum)
          refresh.dropInPlace(refresh.size - n)
          it.copy(guard = Some(πʹ), sum = sumʹ) -> true

        case it @ !(_, _, Some(π @ π(_, λ(ch: Symbol), λ(arg: Symbol), None, _, given Option[Code])), _) =>
          val πʹ = π.copy(channel = renamed(ch), name = renamed(arg), code = recoded)(π.id)
          it.copy(guard = Some(πʹ))

        case it @ !(_, _, Some(π @ π(_, λ(ch: Symbol), _, None, _, given Option[Code])), _) =>
          val πʹ = π.copy(channel = renamed(ch), code = recoded)(π.id)
          it.copy(guard = Some(πʹ))

        case it @ !(_, _, Some(ζ @ ζ(_, name, _, _, given Option[Code])), _) =>
          val ζʹ = ζ.copy(name = renamed(Symbol(name)).asSymbol.name, code = recoded)(ζ.id)
          it.copy(guard = Some(ζʹ))

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

        case it @ `(*)`(_, params*) =>
          val paramsʹ: Seq[λ] = params
            .map {
              case λ(it: Symbol) => renamed(it)
              case it => it
            }
          it.copy(params = paramsʹ)

        case it => it

      }
