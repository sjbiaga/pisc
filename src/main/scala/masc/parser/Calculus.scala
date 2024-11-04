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

import Expression.Code
import Ambient._
import Calculus._


abstract class Calculus extends Ambient:

  def line: Parser[Either[Bind, Encoding]] =
    equation ^^ { Left(_) } | definition ^^ { Right(_) }

  def equation: Parser[Bind] =
    invocation(true)<~"=" >> {
      case (bind, _binding) =>
        _code = -1
        given Names2 = Names2() ++ _binding.map(_ -> Occurrence(None, pos()))
        parallel ^^ {
          case (par, free) =>
            val binding = Names() ++ given_Names2.keys
            if (free &~ binding).nonEmpty
            then
              throw EquationFreeNamesException(bind.identifier, free &~ binding)
            bind -> par.flatten
        }
    }

  def definition: Parser[Encoding] =
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
            val defn = Definition(parameters.toList, constants, variables, given_Names2, par)
            Encoding(_code, term, Some(defn), Nil, constants, variables)
        }
    }

  def parallel(using binding2: Names2): Parser[(||, Names)] =
    rep1sep(sequential, "|") ^^ { ss =>
      ||(ss.map(_._1)*) -> ss.map(_._2).reduce(_ ++ _)
    }

  def sequential(using binding2: Names2): Parser[(`.`, Names)] =
    given Names2 = Names2(binding2)
    prefixes ~ opt( leaf | "("~>parallel<~")" ) ^^ { `pre ~ opt` =>
      binding2 ++= given_Names2.filter(_._2.isBinding < 0)
      `pre ~ opt` match
        case pre ~ Some((end, free)) =>
          `.`(end, pre._1*) -> (pre._2._2 ++ (free &~ pre._2._1))
        case pre ~ _ =>
          `.`(∅, pre._1*) -> pre._2._2 // void
    }

  def leaf(using binding2: Names2): Parser[(-, Names)] =
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
      case _ ~ Some(((Left(enums), _), _)) =>
        throw TermParsingException(enums)
      case (path, free) ~ Some(((Right(it), _), free2)) =>
        <>(Some(it), path*) -> (free ++ free2)
      case (path, free) ~ _ =>
        <>(None, path*) -> free
    } |
    "go" ~> name ~ ("."~> parallel) ^^ { // objective move
      case (amb, name) ~ (par, free) =>
        `go.`(amb, par) -> (name ++ free)
    } |
    IDENT ~ pointers ^^ {
      case id ~ ps =>
        `{}`(id, ps._1) -> ps._2
    } |
    IDENT <~"{"<~"}" ^^ {
      case id =>
        `{}`(id, Nil) -> Names()
    } |
    invocation() |
    instantiation

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
        case (encoding @ Encoding(_, None, _, _, _, _)) :: Nil =>
          (parallel <~ s"$grp1⟧") ~ opt( pointers ) ^^ {
            case (par, free) ~ ps =>
              (`⟦⟧`(encoding, par) -> free) -> ps
          }
        case it =>
          (instance(it, s"$grp1⟧") <~ s"$grp1⟧") ~ opt( pointers ) ^^ {
            case exp ~ ps =>
              exp -> ps
          }
    } ^^ {
      case ((exp @ `⟦⟧`(Encoding(_, _, _, _, constants, variables), _, _), free), _pointers) =>
        nest(false)
        given MutableList[(String, String)]()
        val pointers = _pointers.map(_._1.map(renamed(_))).getOrElse(Nil)
        val _assign = variables zip pointers
        val assign = if _assign.isEmpty then None else Some(_assign)
        Expression.renaming = Some((given_MutableList_String_String, given_Names2))
        given Names = Names()
        val exp2 = exp.copy(assign = assign).rename(free)
        binding2 ++= given_Names2.filter(_._2.isBinding < 0)
        exp2 -> (free ++ constants)
    }

  def instance(defs: List[Encoding], end: String)(using Names2): Parser[(`⟦⟧`, Names)]

  def pointers: Parser[(List[String], Names)] =
    "{"~>rep1sep(name, ",")<~"}" ^^ { ps =>
        ps.map(_._1) -> ps.map(_._2).reduce(_ ++ _)
    }

  def prefixes(using binding2: Names2): Parser[(List[Pre], (Names, Names))] =
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

  def prefix(using binding2: Names2): Parser[(Pre, (Names, Names))] =
    "ν"~>"("~>rep1sep(name, ",")<~")" ^^ { ns => // restriction
      val binding = ns.map(_._2).reduce(_ ++ _)
      Names2(binding)
      ν(ns.map(_._1)*) -> (binding, Names())
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
    ("("~>name<~")") ~ opt( expression ) <~ "." ^^ { case (name, binding) ~ opt => // input action
      Names2(binding)
      opt match
        case Some(((Left(enums), _), _)) =>
          throw TermParsingException(enums)
        case Some(((Right(it), _), free)) =>
          `()`(Some(it), name) -> (binding, free)
        case _ =>
          `()`(None, name) -> (binding, Names())
    }

  def invocation(binding: Boolean = false): Parser[(`(*)`, Names)] =
    qual ~ IDENT ~ opt( "("~>rep1sep(name, ",")<~")" ) ^^ {
      case qual ~ id ~ _ if binding && qual.nonEmpty =>
        throw EquationQualifiedException(id, qual)
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

  import scala.annotation.tailrec

  type Bind = (`(*)`, ||)

  case class Definition(parameters: List[String],
                        constants: Names,
                        variables: Names,
                        binding2: Names2,
                        par: ||):
    def apply(code: Int, term: Term): (Encoding, ||) =
      given refresh: MutableList[(String, String)] = MutableList()
      val variables2 = variables
        .map { it =>
          val υidυ = it.replaceAll("_υ.*υ", "") + id
          refresh.prepend(it -> υidυ)
          υidυ
        }
      given Names2 = Names2(this.binding2)
      Expression.renaming = Some((refresh, given_Names2))
      given Names = Names()
      val par2 = par.rename(Set.empty, definition = true)
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
      Encoding(code, Some(term), None, shadows, constants, variables2) -> par2

  case class Encoding(code: Int,
                      term: Option[Term],
                      definition: Option[Definition],
                      shadows: List[Option[String]],
                      constants: Names,
                      variables: Names):
    override def toString: String =
      ( term match
          case Some(term) => if code == 0 then s"⟦ $term ⟧" else s"⟦$code $term $code⟧"
          case _ => if code == 0 then s"⟦ ⟧" else s"⟦$code $code⟧"
      ) ++ (if constants.isEmpty then "" else constants.mkString("(", ", ", ")"))
        ++ (if variables.isEmpty then "" else variables.mkString("{", ", ", "}"))

  sealed trait AST extends Any

  case class ||(components: `.`*) extends AST:
    override def toString: String = components.mkString(" | ")

  object ∅ extends ||():
    override def canEqual(that: Any): Boolean =
      that.isInstanceOf[||]

    override def equals(any: Any): Boolean = any match
      case that: || => that.components.size == 0
      case _ => false

    override def toString: String = "()"

  case class `.`(end: &, prefixes: Pre*) extends AST:
    override def toString: String =
      prefixes.mkString(" ") + (if prefixes.isEmpty then "" else " ") + (if ∅ != end && end.isInstanceOf[||]
                                                                         then "(" + end + ")" else end)

  sealed trait Pre extends Any

  case class ν(names: String*) extends AnyVal with Pre: // forcibly
    override def toString: String = names.mkString("ν(", ", ", ")")

  case class τ(code: Option[Code]) extends AnyVal with Pre:
    override def toString: String = "τ."

  case class `,.`(path: Ambient.AST*) extends Pre:
    override def toString: String = path.mkString("", ", ", ".")

  case class `()`(code: Option[Term], name: String) extends Pre:
    override def toString: String = s"($name)."

  case class <>(code: Option[Term], path: Ambient.AST*) extends AST:
    override def toString: String = path.mkString("<", ", ", ">")

  case class !(guard: Option[String], par: ||) extends AST:
    override def toString: String = "!" + guard.map("." + _).getOrElse("") + par

  case class `[]`(amb: String, par: ||) extends AST:
    override def toString: String = amb + (if ∅ == par then " [ ]" else " [ " + par + " ]")

  case class `go.`(amb: String, par: ||) extends AST:
    override def toString: String = "go " + amb + "." + par

  case class `⟦⟧`(encoding: Encoding,
                  par: ||,
                  assign: Option[Set[(String, String)]] = None) extends AST:
    override def toString: String =
      s"""$encoding${assign.map{_.map(_ + "->" + _).mkString("{", ", ", "}")}.getOrElse("")} = $par"""

  case class `{}`(identifier: String,
                  pointers: List[String]) extends AST:
    override def toString: String = s"""$identifier{${pointers.mkString(", ")}}"""

  case class `(*)`(identifier: String,
                   qual: List[String],
                   params: String*) extends AST:
    override def toString: String =
      if params.isEmpty
      then identifier
      else s"$identifier(${params.mkString(", ")})"


  // exceptions

  import Expression.ParsingException
  import Ambient.BindingParsingException

  abstract class EquationParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class EquationQualifiedException(id: String, qual: List[String])
      extends EquationParsingException(s"""A qualified package ${qual.mkString(".")} is present in the left hand side of $id""")

  case class EquationFreeNamesException(id: String, free: Names)
      extends EquationParsingException(s"""The free names (${free.mkString(", ")}) in the right hand side are not formal parameters of the left hand side of $id""")

  case class NoDefinitionException(code: Int)
      extends ParsingException(s"No definition for encoding $code")

  case class DefinitionParametersException(code: Int)
      extends EquationParsingException(s"The parameters, constants, and variables must all be different in the left hand side of encoding $code")

  case class DefinitionFreeNamesException(code: Int, free: Names)
      extends EquationParsingException(s"""The free names (${free.mkString(", ")}) in the right hand side are not formal parameters of the left hand side of encoding $code""")

  import scala.meta.Enumerator

  case class TermParsingException(enums: List[Enumerator])
      extends ParsingException(s"The embedded Scalameta should be a Term, not Enumerator `$enums'")

  case class NoBindingParsingException(name: String)
      extends BindingParsingException(s"No binding for $name")


  // functions

  @tailrec
  def shadow(it: String)
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

  def renamed(it: String)
             (using refresh: MutableList[(String, String)])
             (using binding2: Names2): String =
    refresh.find(_._1 == it) match
      case Some((_, r)) => r
      case _ =>
        binding2.find { case (`it`, Binder(_)) => true case _ => false } match
          case Some((_, Binder(υidυ))) => υidυ
          case _ => shadow(it) match
            case Some(Shadow(it)) => it
            case _ if binding2.contains(it) => it
            case _ => throw NoBindingParsingException(it)

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

  private def _removed(it: String)
                      (using binding2: Names2): Option[Either[String, String]] =
    var name: Option[Either[String, String]] = None
    binding2.filterInPlace {
      case (`it`, Shadow(`it`)) => name = Some(Left(it)); false
      case (it, Shadow(`it`)) => name = Some(Right(it)); false
      case _ => true
    }
    name.map(_.flatMap(_removed(_).orElse(name).get))

  inline def removed(it: String)
                    (using binding2: Names2): String =
    _removed(it).get.fold(identity, identity)


  extension[T <: AST](ast: T)

    def flatten: T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅ => ∅

        case ||(`.`(par: ||), it*) =>
          val lhs = par.flatten
          val rhs = ||(it*).flatten
          ||((lhs.components ++ rhs.components).filterNot(∅ == ||(_))*)

        case ||(seq, it*) =>
          val lhs = ||(seq.flatten)
          val rhs = ||(it*).flatten
          ||((lhs.components ++ rhs.components).filterNot(∅ == ||(_))*)

        case `.`(||(`.`(end, ps*)), it*) =>
          `.`(end, (it ++ ps)*).flatten

        case `.`(end, it*) =>
          `.`(end.flatten, it*)

        case !(None, par) =>
          par.flatten match
            case ||(`.`(end: !)) => end
            case it => `!`(None, it)

        case !(guard, par) =>
          `!`(guard, par.flatten)

        case `[]`(amb, par) =>
          `[]`(amb, par.flatten)

        case `go.`(amb, par) =>
          `go.`(amb, par.flatten)

        case it @ `⟦⟧`(_, par, _) =>
          it.copy(par = par.flatten)

        case _ => ast


    def capitals: Names =

      ast match

        case ∅ => Names()

        case ||(it*) => it.map(_.capitals).reduce(_ ++ _)

        case `.`(end, _*) =>
          end.capitals

        case !(_, par) =>
          par.capitals

        case `[]`(_, par) =>
          par.capitals

        case `go.`(_, par) =>
          par.capitals

        case `⟦⟧`(_, par, _) =>
          par.capitals

        case `{}`(id, _) => Set(id)

        case _ => Names()


    def rename(free: Names, definition: Boolean = false)
              (using binding2: Names2)
              (using refresh: MutableList[(String, String)])
              (using binding: Names): T =

      def rebind(it: String)
                (using binding: Names): String =
        val υidυ = it.replaceAll("_υ.*υ", "") + id
        binding2.find { case (_, Shadow(`it`)) => true case _ => false } match
          case Some((_, occurrence)) if definition && occurrence.isBinding < 0 =>
            binding2 += removed(it) -> Shadow(occurrence, it)
            binding2 += it -> Binder(occurrence, υidυ)
          case Some((_, occurrence)) =>
            binding2 += it -> Shadow(occurrence, υidυ)
          case _ =>
            refresh.prepend(it -> υidυ)
        binding += υidυ
        υidυ

      inline def rename[S <: AST](ast: S)(using Names): S = ast.rename(free, definition)

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅ => ∅

        case ||(it*) =>
          ||(it.map(rename(_))*)

        case `.`(end, _it*) =>
          val n = refresh.size
          given Names = Names(binding)
          val it = _it.map {
            case it @ τ(given Option[Code]) =>
              it.copy(code = recoded(free))
            case `,.`(_path*) =>
              val path = _path.map {
                case Λ(name) => Λ(renamed(name))
                case ζ(op, amb) => ζ(op, renamed(amb))
                case it => it
              }
              `,.`(path*)
            case it @ `()`(_, name) =>
              it.copy(name = rebind(name))
            case ν(names*) =>
              ν(names.map(rebind(_))*)
          }
          val end2 = rename(end)
          refresh.dropInPlace(refresh.size - n)
          `.`(end2, it*)

        case <>(it, _path*) =>
          val path = _path.map {
            case Λ(name) => Λ(renamed(name))
            case ζ(op, amb) => ζ(op, renamed(amb))
            case it => it
          }
          <>(it, path*)

        case !(Some(name), par) =>
          val n = refresh.size
          given Names = Names(binding)
          val rep = `!`(Some(rebind(name)), rename(par))
          refresh.dropInPlace(refresh.size - n)
          rep

        case !(guard, par) =>
          `!`(guard, rename(par))

        case `[]`(amb, par) =>
          `[]`(renamed(amb), rename(par))

        case `go.`(amb, par) =>
          `go.`(renamed(amb), rename(par))

        case it @ `⟦⟧`(encoding @ Encoding(_, _, _, _, _, variables), par, assign) =>
          val n = refresh.size
          val assign2 = assign
            .map(
              _.map { (variable, pointer) =>
                val υidυ = variable.replaceAll("_υ.*υ", "") + id
                refresh.prepend(variable -> υidυ)
                val variable2 = renamed(variable)
                variable2 -> renamed(pointer)
              }
            )
          var variables2 = variables
            .drop(assign.map(_.size).getOrElse(0))
            .map { it =>
              val υidυ = it.replaceAll("_υ.*υ", "") + id
              refresh.prepend(it -> υidυ)
              renamed(it)
            }
          variables2 = assign2.map(_.map(_._1)).getOrElse(Names()) ++ variables2
          val encoding2 = encoding.copy(variables = variables2)
          val par2 = rename(par)
          refresh.dropInPlace(refresh.size - n)
          it.copy(encoding = encoding2, par = par2, assign = assign2)

        case it @ `{}`(_, pointers) =>
          it.copy(pointers = pointers.map(renamed(_)))

        case `(*)`(id, qual, params*) =>
          val params2 = params.map(renamed(_))

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
