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

import scala.meta.{ Enumerator, Term }

import scala.util.parsing.combinator._

import Expression.Code
import Pi.{ Names, Names2 }
import Calculus._


abstract class Calculus extends Pi:

  def line: Parser[Either[Bind, Define]] =
    equation ^^ { Left(_) } | definition ^^ { Right(_) }

  def equation: Parser[Bind] =
    agent(true)<~"=" >> {
      case (bind, _binding) =>
        _code = -1
        given Names2 = Names2() ++ _binding.map(_ -> (Right(None), cntr(Left(_))))
        choice ^^ {
          case (sum, free) =>
            val binding = Names() ++ given_Names2.keys
            if (free &~ binding).nonEmpty
            then
              throw EquationFreeNamesException(bind.identifier, free &~ binding)
            bind -> sum.flatten
        }
    }

  def definition: Parser[Define] =
    template ~ opt( "("~>rep1sep(name, ",")<~")" ) ~ opt( "{"~>rep1sep(name, ",")<~"}" ) <~"=" >> {
      case (term, parameters) ~ _const ~ _bound =>
        if defn.contains(_code)
        then
          throw DefinitionUniquenessException(_code)
        val const = _const.map(_.map(_._2).reduce(_ ++ _)).getOrElse(Names())
        val bound = _bound.map(_.map(_._2).reduce(_ ++ _)).getOrElse(Names())
        if (parameters & const).nonEmpty
        || (parameters & bound).nonEmpty
        || (bound & const).nonEmpty
        then
          throw DefinitionParametersException(_code)
        val binding = parameters ++ const ++ bound
        given Names2 = Names2() ++
                       binding
                         .filterNot(_.name.charAt(0).isUpper)
                         .map { it => it -> (Right(None), (if parameters.contains(it) then cntr_ else cntr)(Left(_))) }
        choice ^^ {
          case (_sum, _free) =>
            val sum = _sum.flatten
            val free = _free ++ sum.capitals
            if (free &~ binding).nonEmpty
            then
              throw DefinitionFreeNamesException(_code, free &~ binding)
            given renaming: MutableList[(Symbol, λ)] = MutableList()
            val bound2 = bound
              .map { it =>
                val υidυ = Symbol(it.name.replaceAll("_υ.*υ", "") + id)
                renaming.prepend(it -> λ(υidυ))
                υidυ
              }
            given Names = Names()
            Expression.renaming = Some((renaming, given_Names2))
            val sum2 = sum.rename(Set.empty, definition = true)
            val parameters2 = parameters.filterNot(_.name.charAt(0).isUpper).toList
            if parameters.size == parameters2.size
            then
              val binding = parameters ++ const ++ bound2
              eqtn :+= `(*)`("Self_" + _code, Nil, binding.map(λ(_)).toSeq*) -> sum2
            val binding2 = given_Names2.filter(_._2._2.getOrElse(0L) < 0)
            val paramsMap =
              parameters2
                .map(_ -> None)
                .toMap
              ++
              binding2
                .filter(_._2._1.isLeft)
                .toList
                .map(_ -> _._1.swap.toOption)
            val params = paramsMap.toList
              .sortBy { (it, _) =>
                binding2.find { case (_, (Right(Some(`it`)), _)) => true case _ => false } match
                  case Some((it, _)) => parameters2.indexOf(it)
                  case _ => parameters2.indexOf(it)
              }
            Encoding(_code, term, params.map(_._2), const, bound2) -> sum2
        }
    }

  def choice(using Names2): Parser[(+, Names)] =
    rep1sep(parallel, "+") ^^ { ps =>
      `+`(ps.map(_._1)*) -> ps.map(_._2).reduce(_ ++ _)
    }

  def parallel(using Names2): Parser[(||, Names)] =
    rep1sep(sequential, "|") ^^ { ss =>
      ||(ss.map(_._1)*) -> ss.map(_._2).reduce(_ ++ _)
    }

  def sequential(using binding2: Names2): Parser[(`.`, Names)] =
    given Names2 = Names2(binding2)
    prefixes ~ opt( leaf | "("~>choice<~")" ) ^^ { `pre ~ opt` =>
      binding2 ++= given_Names2.filter(_._2._2.isRight)
      `pre ~ opt` match
        case pre ~ Some((end, free)) =>
          `.`(end, pre._1*) -> (pre._2._2 ++ (free &~ pre._2._1))
        case pre ~ _ =>
          `.`(∅, pre._1*) -> pre._2._2 // inaction
    }

  def leaf(using Names2): Parser[(`-`, Names)] =
    "["~test~"]"~choice ^^ { // (mis)match
      case _ ~ cond ~ _ ~ t =>
        ?:(cond._1, t._1, None) -> (cond._2 ++ t._2)
    } |
    "if"~test~"then"~choice~"else"~choice ^^ { // if then else
      case _ ~ cond ~ _ ~ t ~ _ ~ f =>
        ?:(cond._1, t._1, Some(f._1)) -> (cond._2 ++ (t._2 ++ f._2))
    } |
    test~"?"~choice~":"~choice ^^ { // Elvis operator
      case cond ~ _ ~ t ~ _ ~ f =>
        ?:(cond._1, t._1, Some(f._1)) -> (cond._2 ++ (t._2 ++ f._2))
    } |
    "!"~> opt( "."~>`μ.`<~"." ) >> { // [guarded] replication
      case Some(π @ (π(λ(ch: Symbol), λ(par: Symbol), true, _), _)) =>
        if ch == par
        then
          if _werr
          then
            throw GuardParsingException(ch.name)
          Console.err.println("Warning! " + GuardParsingException(ch.name).getMessage + ".")
        val binding = π._2._1
        Names2(binding)
        choice ^^ {
          case (sum, free) =>
            `!`(Some(π._1), sum) -> ((free &~ binding) ++ π._2._2)
        }
      case Some(μ) =>
        choice ^^ {
          case (sum, free) =>
            `!`(Some(μ._1), sum) -> (free ++ μ._2._2)
        }
      case _ =>
        choice ^^ {
          case (sum, free) =>
            `!`(None, sum) -> free
        }
    } |
    IDENT ~ ("{"~>rep1sep(name, ",")<~"}") ^^ { // pointed values
      case id ~ pointers =>
        `{}`(id, pointers.map(_._1.asSymbol)) -> pointers.map(_._2).reduce(_ ++ _)
    } |
    agent() | // invocation
    instantiation

  def instantiation(using binding2: Names2): Parser[(`⟦⟧`, Names)] =
    given Names2 = Names2(binding2)
    regexMatch("""⟦(\d*)""".r) >> { m =>
      nest(true)
      val grp1 = m.group(1)
      val code = if grp1.isEmpty then 0 else grp1.toInt
      defn(code) match
        case it @ (Encoding(_, Some(_), _, _, _), _) =>
          (instance(it, s"$grp1⟧") <~ s"$grp1⟧") ~ opt( "{"~>rep1sep(name, ",")<~"}" ) ^^ {
            case exp ~ pointers =>
              exp -> pointers
          }
        case (encoding, _) =>
          (choice <~ s"$grp1⟧") ~ opt( "{"~>rep1sep(name, ",")<~"}" ) ^^ {
            case (sum, free) ~ pointers =>
              (`⟦⟧`(encoding, sum) -> free) -> pointers
          }
    } ^^ {
      case ((it @ `⟦⟧`(Encoding(_, _, _, const, bound), _, _), free), _pointers) =>
        nest(false)
        binding2 ++= given_Names2.filter(_._2._2.isRight)
        given MutableList[(Symbol, λ)]()
        val pointers = _pointers.map(_.map(_._1.asSymbol).map(renamed(_).asSymbol)).getOrElse(Nil)
        val _assign = bound zip pointers
        val assign = if _assign.isEmpty then None else Some(_assign)
        Expression.renaming = Some((given_MutableList_Symbol_λ, given_Names2))
        given Names = Names()
        it.copy(assign = assign).rename(free) -> (free ++ const)
    }

  def instance(it: Define, end: String)(using Names2): Parser[(`⟦⟧`, Names)]

  def prefixes(using Names2): Parser[(List[Pre], (Names, Names))] =
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

  def prefix(using Names2): Parser[(Pre, (Names, Names))] =
    "ν"~>"("~>rep1sep(name, ",")<~")" ^^ { // restriction
      case ns if !ns.forall(_._1.isSymbol) =>
        throw PrefixChannelsParsingException(ns.filterNot(_._1.isSymbol).map(_._1)*)
      case ns =>
        val binding = ns.map(_._2).reduce(_ ++ _)
        Names2(binding)
        ν(ns.map(_._1.asSymbol.name)*) -> (binding, Names())
    } |
    `μ.`<~"." ^^ {
      case it @ (_, (binding, _)) =>
        Names2(binding)
        it
    }

  def test: Parser[(((λ, λ), Boolean), Names)] = "("~>test<~")" |
    name~("="|"≠")~name ^^ {
      case (lhs, free_lhs) ~ mismatch ~ (rhs, free_rhs) =>
        (lhs -> rhs -> (mismatch != "=")) -> (free_lhs ++ free_rhs)
    }

  def agent(binding: Boolean = false): Parser[(`(*)`, Names)] =
    qual ~ IDENT ~ opt( "("~>rep1sep(name, ",")<~")" ) ^^ {
      case qual ~ id ~ _ if binding && qual.nonEmpty =>
        throw EquationQualifiedException(id, qual)
      case _ ~ id ~ Some(params) if binding && !params.forall(_._1.isSymbol) =>
        throw EquationParamsException(id, params.filterNot(_._1.isSymbol).map(_._1.value)*)
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

  type Bind = (`(*)`, +)

  type Define = (Encoding, +)

  case class Encoding(code: Int,
                      term: Option[Term],
                      binding: List[Option[Symbol]], // original binding
                      const: Names,
                      bound: Names):
    override def toString: String =
      ( term match
          case Some(term) => if code == 0 then s"⟦ $term ⟧" else s"⟦$code $term $code⟧"
          case _ => if code == 0 then s"⟦ ⟧" else s"⟦$code $code⟧"
      ) ++ (if const.isEmpty then "" else const.map(_.name).mkString("(", ", ", ")"))
        ++ (if bound.isEmpty then "" else bound.map(_.name).mkString("{", ", ", "}"))

  sealed trait AST extends Any

  case class +(choices: || *) extends AST:
    override def toString: String = choices.mkString(" + ")

  object ∅ extends +():
    override def canEqual(that: Any): Boolean =
      that.isInstanceOf[+]

    override def equals(any: Any): Boolean = any match
      case that: + => that.choices.isEmpty
      case _ => false

    override def toString: String = "()"

  case class ||(components: `.`*) extends AnyVal with AST:
    override def toString: String = components.mkString(" | ")

  case class `.`(end: &, prefixes: Pre*) extends AST:
    override def toString: String =
      prefixes.mkString(" ") + (if prefixes.isEmpty then "" else " ") + (if ∅ != end && end.isInstanceOf[+]
                                                                         then "(" + end + ")" else end)

  sealed trait Pre extends Any

  case class ν(names: String*) extends AnyVal with Pre: // forcibly
    override def toString: String = names.mkString("ν(", ", ", ")")

  case class τ(code: Option[Code]) extends AnyVal with Pre:
    override def toString: String = "τ."

  case class π(channel: λ, name: λ, polarity: Boolean, code: Option[Code]) extends Pre:
    override def toString: String =
      if polarity
      then "" + channel + "(" + name + ")."
      else "" + channel + "<" + name + ">."

  case class ?:(cond: ((λ, λ), Boolean), t: +, f: Option[+]) extends AST:
    override def toString: String =
      val test = "" + cond._1._1 + (if cond._2 then " ≠ " else " = ") + cond._1._2
      if f.isEmpty
      then
        "[ " + test + " ] " + t
      else
        "if " + test + " " + t + " else " + f.get

  case class !(guard: Option[μ], sum: +) extends AST:
    override def toString: String = "!" + guard.map("." + _).getOrElse("") + sum

  case class `⟦⟧`(encoding: Encoding,
                  sum: +,
                  assign: Option[Set[(Symbol, Symbol)]] = None) extends AST:
    override def toString: String =
      s"""$encoding${assign.map{_.map(_.name + " = " + _.name).mkString("{", ", ", "}")}.getOrElse("")} = $sum"""

  case class `{}`(identifier: String,
                  pointers: List[Symbol]) extends AST:
    override def toString: String = s"""$identifier{${pointers.map(_.name).mkString(", ")}}"""

  case class `(*)`(identifier: String,
                   qual: List[String],
                   params: λ*) extends AST:
    override def toString: String =
      if params.isEmpty
      then identifier
      else s"$identifier(${params.mkString(", ")})"

  case class λ(value: Any):
    val isSymbol: Boolean = value.isInstanceOf[Symbol]
    def asSymbol: Symbol = value.asInstanceOf[Symbol]

    type Kind = value.type match
      case Symbol => Symbol
      case BigDecimal => BigDecimal
      case Boolean => Boolean
      case String => String
      case Expr => Expr

    val kind: String = value match
      case _: Symbol => "channel name"
      case _: BigDecimal => "decimal number"
      case _: Boolean => "True False"
      case _: String => "string literal"
      case _: Expr => "Scalameta Term"

    override def toString: String = value match
      case it: Symbol => it.name
      case it: BigDecimal => "" + it
      case it: Boolean => it.toString.capitalize
      case it: String => it
      case it: Expr => "" + it

  case class Expr(term: Term):
    override def toString: String = "/*" + term + "*/"


  // exceptions

  import Expression.ParsingException
  import Pi.{ PrefixParsingException, BindingParsingException }

  abstract class EquationParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class EquationQualifiedException(id: String, qual: List[String])
      extends EquationParsingException(s"A qualified package ${qual.mkString(".")} is present in the left hand side of $id")

  case class EquationParamsException(id: String, params: Any*)
      extends EquationParsingException(s"The \"formal\" parameters (${params.mkString(", ")}) are not names in the left hand side of $id")

  case class EquationFreeNamesException(id: String, free: Names)
      extends EquationParsingException(s"The free names (${free.map(_.name).mkString(", ")}) in the right hand side are not formal parameters of the left hand side of $id")

  case class DefinitionUniquenessException(code: Int)
      extends EquationParsingException(s"The encoding $code has already been given a definition")

  case class DefinitionParametersException(code: Int)
      extends EquationParsingException(s"The parameters, constants and pointers must all be different in the left hand side of encoding $code")

  case class DefinitionFreeNamesException(code: Int, free: Names)
      extends EquationParsingException(s"The free names (${free.map(_.name).mkString(", ")}) in the right hand side are not formal parameters of the left hand side of encoding $code")

  case class PrefixChannelsParsingException(names: λ*)
      extends PrefixParsingException(s"${names.map(_.value).mkString(", ")} are not channel names but ${names.map(_.kind).mkString(", ")}")

  case class GuardParsingException(name: String)
      extends PrefixParsingException(s"$name is both the channel name and the binding parameter name in an input guard")

  case class NoBindingParsingException(name: String)
      extends BindingParsingException(s"No binding for $name")


  // functions

  def renamed(it: Symbol)
             (using renaming: MutableList[(Symbol, λ)])
             (using binding2: Names2): λ =
    renaming.find(_._1 == it) match
      case Some((_, r)) => r
      case _ if binding2.exists { case (`it`, (Left(_), _)) | (_, (Right(Some(`it`)), _)) => true case _ => false } =>
        binding2.find { case (`it`, (Left(_), _)) => true case _ => false } match
          case Some((_, (Left(υidυ), _))) => λ(υidυ)
          case _ => λ(it)
      case _ if binding2.contains(it) => λ(it)
      case _ => throw NoBindingParsingException(it.name)

  def aliased(it: Symbol)
             (using renaming: MutableList[(Symbol, λ)])
             (using binding2: Names2): Option[Symbol] =
    renaming.find(_._2.asSymbol == it) match
      case Some((r, _)) => Some(r)
      case _ if binding2.exists { case (`it`, (Left(_), _)) | (_, (Right(Some(`it`)), _)) => true case _ => false } =>
        binding2.find { case (`it`, (Left(_), _)) => true case _ => false } match
          case Some((_, (Left(υidυ), _))) => Some(υidυ)
          case _ => Some(it)
      case _ if binding2.contains(it) => Some(it)
      case _ => None

  def recoded(free: Names)
             (using code: Option[Code])
             (using MutableList[(Symbol, λ)])
             (using Names2)
             (using binding: Names): Option[Code] =
    code.map { (_, orig) =>
      val term = Expression(orig)._1
      var (code2, names) = Expression.recode(term)
      names = names.map(aliased(_)).filter(_.nonEmpty).map(_.get)
      free ++= names.filterNot(binding.contains(_))
      code2
    }


  extension[T <: AST](ast: T)

    def flatten: T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅ => ∅

        case +(||(`.`(sum: +)), it*) =>
          val lhs = sum.flatten
          val rhs = `+`(it*).flatten
          `+`((lhs.choices ++ rhs.choices).filterNot(∅ == `+`(_))*)

        case +(par, it*) =>
          val lhs = `+`(par.flatten)
          val rhs = `+`(it*).flatten
          `+`((lhs.choices ++ rhs.choices).filterNot(∅ == `+`(_))*)

        case ||(`.`(+(par)), it*) =>
          val lhs = par.flatten
          val rhs = ||(it*).flatten
          ||((lhs.components ++ rhs.components)*)

        case ||(seq, it*) =>
          val lhs = ||(seq.flatten)
          val rhs = ||(it*).flatten
          ||((lhs.components ++ rhs.components)*)

        case `.`(+(||(`.`(end, ps*))), it*) =>
          `.`(end, (it ++ ps)*).flatten

        case `.`(end, it*) =>
          `.`(end.flatten, it*)

        case ?:(cond, t, f) =>
          ?:(cond, t.flatten, f.map(_.flatten))

        case !(None, sum) =>
          sum.flatten match
            case +(||(`.`(end: !))) => end
            case it => `!`(None, it)

        case !(μ, sum) =>
          `!`(μ, sum.flatten)

        case it @ `⟦⟧`(_, sum, _) =>
          it.copy(sum = sum.flatten)

        case _ => ast


    def capitals: Names =

      ast match

        case ∅ => Set.empty

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

        case `{}`(id, _) => Set(Symbol(id))

        case `(*)`(id, Nil) => Set(Symbol(id))

        case _ => Set.empty


    def rename(free: Names, definition: Boolean = false)
              (using binding2: Names2)
              (using renaming: MutableList[(Symbol, λ)])
              (using binding: Names): T =

      def rebind(it: Symbol)(using binding: Names): λ =
        binding2.find { case (_, (Right(Some(`it`)), _)) => true case _ => false } match
          case Some((name, (_, Right(k)))) if definition && k < 0 =>
            val υidυ = Symbol(it.name.replaceAll("_υ.*υ", "") + id)
            binding2 += name -> (Right(Some(it)), Right(k))
            binding2 += it -> (Left(υidυ), Right(k))
            binding += υidυ
            λ(υidυ)
          case _ =>
            if definition || binding2.get(it).map(_._2.isLeft).getOrElse(true)
            then
              val υidυ = Symbol(it.name.replaceAll("_υ.*υ", "") + id)
              renaming.prepend(it -> λ(υidυ))
              binding += it
              λ(υidυ)
            else
              renamed(it)

      inline def rename[S <: AST](ast: S)(using Names): S = ast.rename(free, definition)

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅ => ∅

        case +(it*) =>
          `+`(it.map(rename(_))*)

        case ||(it*) =>
          ||(it.map(rename(_))*)

        case `.`(end, _it*) =>
          val n = renaming.size
          given Names = Names(binding)
          val it = _it.map {
            case it @ τ(given Option[Code]) =>
              it.copy(code = recoded(free))
            case it @ π(λ(ch: Symbol), λ(par: Symbol), true, given Option[Code]) =>
              it.copy(channel = renamed(ch), name = rebind(par), code = recoded(free))
            case it @ π(λ(ch: Symbol), λ(arg: Symbol), false, given Option[Code]) =>
              it.copy(channel = renamed(ch), name = renamed(arg), code = recoded(free))
            case it @ π(λ(ch: Symbol), _, false, given Option[Code]) =>
              it.copy(channel = renamed(ch), code = recoded(free))
            case ν(_names*) =>
              val names = _names.map(Symbol(_)).map(rebind(_))
              ν(names.map(_.asSymbol.name)*)
            case it => it
          }
          val end2 = rename(end)
          renaming.dropInPlace(renaming.size - n)
          `.`(end2, it*)

        case ?:(((λ(lhs: Symbol), λ(rhs: Symbol)), m), t, f) =>
          ?:(((renamed(lhs), renamed(rhs)), m), rename(t), f.map(rename(_)))

        case ?:(((λ(lhs: Symbol), rhs), m), t, f) =>
          ?:(((renamed(lhs), rhs), m), rename(t), f.map(rename(_)))

        case ?:(((lhs, λ(rhs: Symbol)), m), t, f) =>
          ?:(((lhs, renamed(rhs)), m), rename(t), f.map(rename(_)))

        case ?:(cond, t, f) =>
          ?:(cond, rename(t), f.map(rename(_)))

        case !(Some(it @ π(λ(ch: Symbol), λ(par: Symbol), true, given Option[Code])), sum) =>
          val n = renaming.size
          given Names = Names(binding)
          val π = it.copy(channel = renamed(ch), name = rebind(par), code = recoded(free))
          val sum2 = rename(sum)
          renaming.dropInPlace(renaming.size - n)
          `!`(Some(π), sum2)

        case !(Some(it @ π(λ(ch: Symbol), λ(arg: Symbol), false, given Option[Code])), sum) =>
          val π = it.copy(channel = renamed(ch), name = renamed(arg), code = recoded(free))
          `!`(Some(π), rename(sum))

        case !(Some(it @ π(λ(ch: Symbol), _, false, given Option[Code])), sum) =>
          val π = it.copy(channel = renamed(ch), code = recoded(free))
          `!`(Some(π), rename(sum))

        case !(guard, sum) =>
          `!`(guard, rename(sum))

        case it @ `⟦⟧`(encoding @ Encoding(_, _, _, _, bound), sum, assign) =>
          val n = renaming.size
          val assign2 = assign
            .map(
              _.map { (variable, pointer) =>
                val pointer2 = renamed(pointer).asSymbol
                val υidυ = Symbol(variable.name.replaceAll("_υ.*υ", "") + id)
                renaming.prepend(variable -> λ(υidυ))
                υidυ -> pointer2
              }
            )
          var bound2 = bound
            .drop(assign.map(_.size).getOrElse(0))
            .map { it =>
              val υidυ = Symbol(it.name.replaceAll("_υ.*υ", "") + id)
              renaming.prepend(it -> λ(υidυ))
              υidυ
            }
          bound2 = assign2.map(_.map(_._1)).getOrElse(Names()) ++ bound2
          val encoding2 = encoding.copy(bound = bound2)
          val sum2 = rename(sum)
          renaming.dropInPlace(renaming.size - n)
          it.copy(encoding = encoding2, sum = sum2, assign = assign2)

        case it @ `{}`(_, pointers) =>
          it.copy(pointers = pointers.map(renamed(_).asSymbol))

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
