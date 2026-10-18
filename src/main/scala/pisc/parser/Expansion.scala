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

package scala.util.parsing.combinator
package pisc
package parser

import java.util.regex.Pattern
import scala.util.matching.Regex
import Regex.Match

import scala.collection.mutable.{ LinkedHashMap => Map }

import scala.meta.{ Lit, Term }

import _root_.pisc.parser.Expression
import Expression.Code
import _root_.pisc.parser.Encoding
import _root_.pisc.parser.Pi.*
import _root_.pisc.parser.Calculus.*
import Encoding.*
import Expansion.*


abstract class Expansion extends Encoding:

  /**
    * A parser that matches a regex string and returns the Match
    * [[https://stackoverflow.com/questions/1815716/accessing-scala-parser-regular-expression-match-data]]
    */
  def regexMatch(r: Regex): Parser[Regex.Match] = new Parser[Regex.Match] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      (r findPrefixMatchOf SubSequence(source, start)) match {
        case Some(matched) =>
          Success(matched, in.drop(start + matched.end - offset))
        case None =>
          Failure("string matching regex `"+r+"' expected but `"+in.first+"' found", in.drop(start - offset))
      }
    }
  }


  override def instance(defs: List[Define], end: String)
                       (using Bindings, Duplications, Int): Parser[`⟦⟧`] =
    var idx = -1
    val xid = χ_id
    given Boolean = emitter == Emitter.go

    new Parser[`⟦⟧`] {

      def expand(in: Input, shadows: List[Option[Symbol]], key: CacheKey)
                (_op: Either[String, Lit], end: Either[String, String])
                (success: Input => (ParseResult[Fresh], Seq[Term]))
                (using bindings: Bindings)
                (using duplications: Duplications)
                (using substitution: Substitution)
                (using free: Names): (ParseResult[Fresh], Seq[Term]) =

        def bf(freeʹ: Names)(using Bindings): Unit =
          bindings ++= cleaned
          free ++= freeʹ &~ bindings.keySet

        def pf(freeʹ: Names)(using Bindings): Unit =
          PendingOccurrence(freeʹ)
          free ++= freeʹ &~ bindings.keySet

        val source = in.source
        val offset = in.offset
        val start = handleWhiteSpace(source, offset)

        if (open_r findPrefixMatchOf SubSequence(source, start, 1 min (source.length - start))).nonEmpty
        then

          _op match

            case Right(lit) =>
              Failure(s"literal `$lit` expected not instantiation", in) -> Nil

            case Left(op) =>

              if op.charAt(0).isLower
              then
                Failure("name expected not instantiation", in) -> Nil

              else if op == "_"
              then
                Failure("nothing expected not instantiation", in) -> Nil

              else

                _cache.get(key) match

                  case Some((exp: `⟦⟧`, cp, given Bindings, inʹ)) =>
                    bf(exp.free)

                    substitution(op) = exp

                    paste(cp)

                    success(inʹ)

                  case _ =>

                    given Bindings = Bindings(bindings)
                    parse(positioned(instantiation), in) match

                      case Success(exp, in) =>
                        val source = in.source
                        val offset = in.offset
                        val start = handleWhiteSpace(source, offset)

                        val n = end.map(_.length).getOrElse(0)

                        if start + n <= source.length
                        && (n == 0 || SubSequence(source, start, n).toString == end.right.get)
                        then
                          bf(exp.free)

                          substitution(op) = exp

                          val inʹ = in.drop(start + n - offset)

                          _cache(key) = (exp, copy, given_Bindings, inʹ)

                          success(inʹ)

                        else
                          Failure(s"operator '${end.right.get}' expected", in) -> Nil

                      case _ =>
                        Failure("instantiation expected", in) -> Nil

        else balanced(end)(source, start) match

          case Some(matched) =>
            var n = matched.end - end.orElse(end.swap).map(_.length).right.get
            val result = SubSequence(source, start, n).toString

            n += end.map(_.length).getOrElse(0)

            if result.isBlank
            || result.strip == "_"
            then

              _op match

                case Right(lit) =>
                  Failure(s"literal `$lit` expected", in) -> Nil

                case Left(op) =>

                  if op.charAt(0).isUpper
                  then
                    substitution(op) = ∅().setPos(in.drop(start - offset).pos)

                    val inʹ = in.drop(start + n - offset)
                    success(inʹ)

                  else if op == "_"
                  then
                    val inʹ = in.drop(start + n - offset)
                    success(inʹ)

                  else
                    Failure("name expected", in) -> Nil

            else

              _op match

                case Right(lit) =>

                  import scala.meta.*
                  import scala.meta.contrib.*
                  import dialects.Scala3

                  result.parse[Term] match

                    case scala.meta.parsers.Parsed.Success(litʹ) if lit isEqual litʹ =>
                      val inʹ = in.drop(start + n - offset)
                      success(inʹ)

                    case _ =>
                      Failure(s"literal `$lit` expected but `${result.strip}` found", in) -> Nil

                case Left(op) =>

                  if op.charAt(0).isLower
                  then

                    parseAll(name, result) match

                      case Success((λ(it: Symbol), freeʹ), _) =>
                        shadows(idx) match
                          case shadow @ Some(_) =>
                            BindingOccurrence(it, shadow)
                            duplications(xid)._2(op) = it -> shadow
                          case _ =>
                            bindings.find { case (`it`, Shadow(_)) => true case _ => false } match
                              case Some((_, Shadow(υidυ))) =>
                                PendingOccurrence(freeʹ)
                                substitution(op) = λ(υidυ)
                              case _ =>
                                pf(freeʹ)
                                substitution(op) = λ(it)
                        idx += 1

                        val inʹ = in.drop(start + n - offset)
                        success(inʹ)

                      case Success((it, freeʹ), _) =>
                        pf(freeʹ)
                        idx += 1

                        substitution(op) = it

                        val inʹ = in.drop(start + n - offset)
                        success(inʹ)

                      case _ =>
                        Failure("name expected", in) -> Nil

                  else if op == "_"
                  then
                    Failure("nothing expected", in) -> Nil

                  else

                    _cache.get(key) match

                      case Some((sum: +, cp, given Bindings, inʹ)) =>
                        bf(sum.free)

                        substitution(op) = sum

                        paste(cp)

                        success(inʹ)

                      case _ =>

                        given Bindings = Bindings(bindings)
                        parseAll(positioned(choice), result) match

                          case Success(sum, _) =>
                            bf(sum.free)

                            val sumʹ = sum.flatten.update(using Bindings(given_Bindings))

                            val colʹ = in.drop(start - offset).pos.column

                            extension (self: AST)
                              def shift: Unit =
                                val col = self.pos.column
                                self.pos =
                                  new scala.util.parsing.input.Position {
                                    override val line: Int = 1
                                    override val column: Int = colʹ + col - 1
                                    override protected val lineContents: String = "N/A"
                                  }

                            sumʹ.foreach(_.shift)

                            substitution(op) = sumʹ

                            val inʹ = in.drop(start + n - offset)

                            _cache(key) = (sumʹ, copy, given_Bindings, inʹ)

                            success(inʹ)

                          case _ =>
                            Failure("choice expected", in) -> Nil

          case _ =>
            val found = if start == source.length
                        then "end of source"
                        else "'"+source.charAt(start)+"'"
            Failure(end.map("operator '" + _ + "'").orElse(end.swap).right.get+" expected but "+found+" found", in.drop(start - offset)) -> Nil


      def expand(in: Input, _ts: Seq[Term], end: Either[String, String])
                (using Bindings, Duplications, Substitution, Names): ((Fresh, Term)) => (ParseResult[Fresh], Seq[Term]) =

        case (it @ (_, (_, shadows)), _rhs @ (Term.Name(_) | Term.Placeholder() | _: Lit)) =>
          val rhs = _rhs match { case lit: Lit => Right(lit) case _ => Left(_rhs.toString) }

          val source = in.source
          val offset = in.offset
          val start = handleWhiteSpace(source, offset)

          val ts = _ts :+ _rhs
          val key = path -> (ts.mkString -> end) -> start

          def success(inʹ: Input) =
            Success(it, inʹ) -> ts

          expand(in, shadows, key)(rhs, end)(success)

        case (it @ (_, (_, shadows)), Term.ApplyInfix(_lhs @ (Term.Name(_) | Term.Placeholder() | _: Lit), _op @ Term.Name(op), _, List(rhs))) =>
          val lhs = _lhs match { case lit: Lit => Right(lit) case _ => Left(_lhs.toString) }

          val source = in.source
          val offset = in.offset
          val start = handleWhiteSpace(source, offset)

          val ts = _ts :+ _lhs :+ _op
          val key = path -> (ts.mkString -> end) -> start

          def success(inʹ: Input) =
            expand(inʹ, ts, end)(it -> rhs)

          expand(in, shadows, key)(lhs, Right(op))(success)

        case (it, Term.ApplyInfix(lhs: Term.ApplyInfix, _op @ Term.Name(op), _, List(rhs))) =>
          expand(in, _ts, Right(op))(it -> lhs) match
            case (Success(_, inʹ), ts) =>
              expand(inʹ, ts :+ _op, end)(it -> rhs)
            case it => it

        case (it, Term.AnonymousFunction(body)) =>
          expand(in, _ts, end)(it -> body)

        case _ => ??? // caught by template


      override def apply(in: Input): ParseResult[`⟦⟧`] =
        val duplications = summon[Duplications]
        duplications += xid -> (false, Map())

        try
          val bindings = summon[Bindings]

          var r: Option[(Fresh, (Substitution, (Names, (Bindings, Duplications))), Input)] = None

          var ls = defs
          while ls.nonEmpty
          do
            val (_macro, Definition(code, Some(term), _, _, _)) = ls.head : @unchecked
            ls = ls.tail

            given Bindings = Bindings(bindings)
            given Duplications = Duplications(duplications)
            given Substitution()
            given Names()
            idx = 0

            save(expand(in, Nil, Left(end))(_macro(code, term, _settings.dups)(id, χ_id) -> term)._1) match
              case Some(_) if r.nonEmpty => throw AmbiguousParsingException
              case Some((it @ (_, (arity, _)), in)) if arity == given_Substitution.size =>
                r = Some((it, given_Substitution -> (given_Names -> (given_Bindings, given_Duplications)), in))
              case _ =>

          r match

            case Some(((definition, _), (given Substitution, (free, (given Bindings, given Duplications))), inʹ)) =>
              given_Duplications --= duplications.keySet
              duplications ++= given_Duplications

              val tooMP = { (code: Int, amount: Int) => warn(throw TooManyPointersParsingException(code, amount)) }

              val exp = definition(_code, _nest, _settings.dups, duplicated, _.replace(tooMP).flatten)(id)(using duplications)

              bindings ++= purged

              Success(exp.copy(xid = xid).free = free, inʹ)

            case _ => throw UndefinedParsingException

        catch t =>
          duplications -= xid

          t match
            case (AmbiguousParsingException | UndefinedParsingException) =>
              Failure(t.getMessage, in)
            case t =>
              throw t

    }.named(s"""⟦${if defs.size == 1 then defs.head._2.code.toString else ""}⟧""")


  protected def duplicated(xid: String)
                          (using Bindings, Int)
                          (using duplications: Duplications): Term => Unit =

    case _rhs @ (Term.Name(_) | Term.Placeholder() | _: Lit) =>
      val rhs = _rhs match { case Term.Name(rhs) => rhs case _ => null }

      duplications(xid) match
        case (true, map) =>
          map.get(rhs) match
            case Some(it -> shadow) =>
              BindingOccurrence(it, shadow)
            case _ =>
        case _ =>

    case Term.ApplyInfix(_lhs @ (Term.Name(_) | Term.Placeholder() | _: Lit), _, _, List(rhs)) =>
      val lhs = _lhs match { case Term.Name(lhs) => lhs case _ => null }

      duplications(xid) match
        case (true, map) =>
          map.get(lhs) match
            case Some(it -> shadow) =>
              BindingOccurrence(it, shadow)
            case _ =>
        case _ =>

      duplicated(xid)(rhs)

    case Term.ApplyInfix(lhs: Term.ApplyInfix, _, _, List(rhs)) =>
      duplicated(xid)(lhs)
      duplicated(xid)(rhs)

    case Term.AnonymousFunction(body) =>
      duplicated(xid)(body)


object Expansion:

  type Duplications = Map[String, (Boolean, Map[String, (Symbol, Option[Symbol])])]

  object Duplications:
    def apply(): Duplications = Map()
    def apply(duplications: Duplications): Duplications = Map.from(duplications)

  type Substitution = Map[String, λ | (+ | `⟦⟧`)]

  private lazy val open_r = """⟦\d*""".r
  private lazy val closed_r = """\d*⟧""".r

  // exceptions

  import _root_.pisc.parser.Expression.ParsingException

  case object AmbiguousParsingException
      extends ParsingException(s"An instantiation of a template is ambiguous")

  case object UndefinedParsingException
      extends ParsingException(s"An instantiation of a template is undefined")


  // functions

  def balanced(end: Either[String, String])
              (source: CharSequence, start: Int): Option[Match] =
    var s = SubSequence(source, start)
    val op = end.orElse(end.swap).right.get
    val n = op.length
    val matches = (Pattern.quote(op).r findAllMatchIn s).toList
    var i = 0
    while i < matches.size
    && { s = SubSequence(source, start, matches(i).end - n); true }
    && (open_r findAllMatchIn s).size > (closed_r findAllMatchIn s).size
    do
      i += 1
    if i < matches.size
    && {
      val os = (open_r findAllMatchIn s).toList
      val cs = (closed_r findAllMatchIn s).toList
      if os.size != cs.size
      then
        false
      else if os.isEmpty
      then
        true
      else
        var oi, ci = 0
        while ci <= oi && oi < os.size
        do
          while oi < os.size && os(oi).start < cs(ci).start
          do
            oi += 1
          if oi < os.size
          then
            while ci < cs.size && cs(ci).start < os(oi).start
            do
              ci += 1
          else
            ci = cs.size
        oi == os.size && ci == cs.size
    }
    then
      Some(matches(i))
    else
      None


  def replaced(name: Symbol)
              (using substitution: Substitution): λ =
    substitution.get(name.name) match
      case Some(it: λ) => it
      case _ => λ(name)

  def replaced(term: Term)
              (using go: Boolean)
              (using Substitution): Term =
    term match
      case it @ (Lit.Int(_) | Lit.Long(_) | Lit.Boolean(_) | Lit.Double(_) | Lit.String(_)) if go => it
      case Term.Name(str) if go =>
        Term.Name {
          Expression.identifiers(str)
            .foldLeft(str) { (r, it) =>
              replaced(it) match
                case λ(Symbol(name)) =>
                  r.replaceAll(s"'${it.name}", s"'$name")
                case value =>
                  r.replaceAll(s"'${it.name}", value.toString)
            }
        }
      case _ =>
        Expression(term)._1

  def updated(name: Symbol)
             (using bindings: Bindings): Symbol =
    bindings.find { case (`name`, Shadow(_)) => true case _ => false } match
      case Some((_, Shadow(it))) => it
      case _ => name

  def updated(term: Term)
             (using go: Boolean)
             (using Bindings): Term =
    term match
      case it @ (Lit.Int(_) | Lit.Long(_) | Lit.Boolean(_) | Lit.Double(_) | Lit.String(_)) if go => it
      case Term.Name(str) if go =>
        Term.Name {
          Expression.identifiers(str)
            .foldLeft(str) { (r, it) =>
              r.replaceAll(s"'${it.name}", s"'${updated(it).name}")
            }
        }
      case _ =>
        Expression(term)._1

  def recoded(using code: Option[Code])
             (using go: Boolean)
             (using substitution: Substitution = null)
             (using updating: Bindings = null): Option[Code] =
    code.map {
      case it @ (_, Lit.Int(_) | Lit.Long(_) | Lit.Boolean(_) | Lit.Double(_) | Lit.String(_)) if go => it
      case (_, it: Term.Name) if go && (substitution ne null) =>
        val lit = replaced(it)
        (Right(lit), lit)
      case (_, it: Term.Name) if go && (updating ne null) =>
        val lit = updated(it)
        (Right(lit), lit)
      case (_, orig) =>
        val term = Expression(orig)._1
        Expression.recode(term)
    }


  given Conversion[Symbol | Term, λ] = λ(_)

  extension [T <: AST](ast: T)

    def replace(tooMP: (Int, Int) => Unit)
               (using Boolean)
               (using substitution: Substitution)
               (using rename: + | `⟦⟧` => + | `⟦⟧`): T =

      ast.mapʹ {

        case it @ `.`(_, prefixes*) =>
          val prefixesʹ = prefixes.map {
            case it @ τ(given Option[Code]) =>
              it.cc(code = recoded)
            case it @ π(λ(ch: Symbol), _, Some(_), given Option[Code]) =>
              it.cc(channel = replaced(ch), code = recoded)
            case it @ π(λ(ch: Symbol), λ(arg: Symbol), None, given Option[Code]) =>
              it.cc(channel = replaced(ch), name = replaced(arg), code = recoded)
            case it @ π(λ(ch: Symbol), λ(term: Term), None, given Option[Code]) =>
              it.cc(channel = replaced(ch), name = replaced(term), code = recoded)
            case it @ π(λ(ch: Symbol), _, None, given Option[Code]) =>
              it.cc(channel = replaced(ch), code = recoded)
            case it => it
          }
          it.cc(prefixes = prefixesʹ)

        case it @ ?:(((λ(lhs: Symbol), λ(rhs: Symbol)), m), _, _) =>
          it.cc(cond = ((replaced(lhs), replaced(rhs)), m))

        case it @ ?:(((λ(lhs: Symbol), rhs), m), _, _) =>
          it.cc(cond = ((replaced(lhs), rhs), m))

        case it @ ?:(((lhs, λ(rhs: Symbol)), m), _, _) =>
          it.cc(cond = ((lhs, replaced(rhs)), m))

        case it @ !(_, _, Some(τ @ τ(given Option[Code])), _) =>
          it.cc(guard = Some(τ.cc(code = recoded)))

        case it @ !(_, _, Some(π @ π(λ(ch: Symbol), _, Some(_), given Option[Code])), _) =>
          it.cc(guard = Some(π.cc(channel = replaced(ch), code = recoded)))

        case it @ !(_, _, Some(π @ π(λ(ch: Symbol), λ(arg: Symbol), None, given Option[Code])), _) =>
          it.cc(guard = Some(π.cc(channel = replaced(ch), name = replaced(arg), code = recoded)))

        case it @ !(_, _, Some(π @ π(λ(ch: Symbol), λ(term: Term), None, given Option[Code])), _) =>
          it.cc(guard = Some(π.cc(channel = replaced(ch), name = replaced(term), code = recoded)))

        case it @ !(_, _, Some(π @ π(λ(ch: Symbol), _, None, given Option[Code])), _) =>
          it.cc(guard = Some(π.cc(channel = replaced(ch), code = recoded)))

        case it @ `⟦⟧`(_, _, _, pointers) =>
          val pointersʹ = pointers.map(replaced(_).asSymbol)
          it.cc(pointers = pointersʹ)

        case `{}`(identifier, pointers, false) =>
          val ast = rename(substitution(identifier).asInstanceOf[+ | `⟦⟧`])
          given List[Symbol] = pointers.map(replaced(_).asSymbol)
          if given_List_Symbol.nonEmpty
          then
            ast.concatenate(tooMP)
          else
            ast

        case it @ `{}`(_, pointers, true, params*) =>
          val pointersʹ = pointers.map(replaced(_).asSymbol)
          val paramsʹ: Seq[λ] = params
            .map {
              case λ(it: Symbol) => replaced(it)
              case it => it
            }
          it.cc(pointers = pointersʹ, params = paramsʹ)

        case _: `{}` => ???

        case it @ `(*)`(_, _, params*) =>
          val paramsʹ: Seq[λ] = params
            .map {
              case λ(it: Symbol) => replaced(it)
              case it => it
            }
          it.cc(params = paramsʹ)

        case it => it

      }

    def concatenate(tooMP: (Int, Int) => Unit)
                   (using pointers: List[Symbol]): T =

      given Conversion[AST, (T, Boolean)] = _.asInstanceOf[T] -> false

      ast.mapʹʹ {

        case it: `⟦⟧` =>
          val pointersʹ = it.pointers ::: pointers
          if pointersʹ.size > it.definition.variables.size
          then
            tooMP(it.definition.code, pointersʹ.size - it.definition.variables.size)
          it.cc(pointers = pointersʹ) -> true

        case it: `{}` =>
          it.cc(pointers = it.pointers ::: pointers)

        case it => it

      }


    def update(using bindings: Bindings)
              (using Boolean): T =

      given Conversion[AST, (T, Boolean)] = _.asInstanceOf[T] -> false

      ast.mapʹʹ {

        case it @ `.`(end, prefixes*) =>
          given Bindings = Bindings(bindings)
          val prefixesʹ = prefixes.map {
            case it @ ν(names*) =>
              given_Bindings --= names.map(Symbol(_))
              it
            case it @ τ(given Option[Code]) =>
              it.cc(code = recoded)
            case it @ π(λ(ch: Symbol), λ(params: List[`λ`]), Some(_), given Option[Code]) =>
              val chʹ = updated(ch)
              given_Bindings --= params.map(_.asSymbol)
              it.cc(channel = chʹ, code = recoded)
            case it @ π(λ(ch: Symbol), λ(par: Symbol), Some(_), given Option[Code]) =>
              val chʹ = updated(ch)
              given_Bindings -= par
              it.cc(channel = chʹ, code = recoded)
            case it @ π(λ(ch: Symbol), λ(arg: Symbol), None, given Option[Code]) =>
              it.cc(channel = updated(ch), name = updated(arg), code = recoded)
            case it @ π(λ(ch: Symbol), λ(term: Term), None, given Option[Code]) =>
              it.cc(channel = updated(ch), name = updated(term), code = recoded)
            case it @ π(λ(ch: Symbol), _, None, given Option[Code]) =>
              it.cc(channel = updated(ch), code = recoded)
            case it => it
          }
          it.cc(end = end.update, prefixes = prefixesʹ) -> true

        case it @ ?:(((λ(lhs: Symbol), λ(rhs: Symbol)), m), _, _) =>
          it.cc(cond = ((updated(lhs), updated(rhs)), m))

        case it @ ?:(((λ(lhs: Symbol), rhs), m), _, _) =>
          it.cc(cond = ((updated(lhs), rhs), m))

        case it @ ?:(((lhs, λ(rhs: Symbol)), m), _, _) =>
          it.cc(cond = ((lhs, updated(rhs)), m))

        case it @ !(_, _, Some(τ @ τ(given Option[Code])), _) =>
          it.cc(guard = Some(τ.cc(code = recoded)))

        case it @ !(_, _, Some(π @ π(λ(ch: Symbol), λ(par: Symbol), Some(_), given Option[Code])), sum) =>
          given Bindings = Bindings(bindings)
          val chʹ = updated(ch)
          given_Bindings -= par
          it.cc(guard = Some(π.cc(channel = chʹ, code = recoded)), sum = sum.update) -> true

        case it @ !(_, _, Some(π @ π(λ(ch: Symbol), λ(arg: Symbol), None, given Option[Code])), _) =>
          it.cc(guard = Some(π.cc(channel = updated(ch), name = updated(arg), code = recoded)))

        case it @ !(_, _, Some(π @ π(λ(ch: Symbol), λ(term: Term), None, given Option[Code])), _) =>
          it.cc(guard = Some(π.cc(channel = updated(ch), name = updated(term), code = recoded)))

        case it @ !(_, _, Some(π @ π(λ(ch: Symbol), _, None, given Option[Code])), _) =>
          it.cc(guard = Some(π.cc(channel = updated(ch), code = recoded)))

        case it @ `⟦⟧`(_, _, _, pointers) =>
          val pointersʹ = pointers.map(updated(_).asSymbol)
          it.cc(pointers = pointersʹ)

        case it @ `{}`(_, pointers, _, params*) =>
          val pointersʹ = pointers.map(updated(_).asSymbol)
          val paramsʹ: Seq[λ] = params
            .map {
              case λ(it: Symbol) => updated(it)
              case it => it
            }
          it.cc(pointers = pointersʹ, params = paramsʹ)

        case it @ `(*)`(_, _, params*) =>
          val paramsʹ: Seq[λ] = params
            .map {
              case λ(it: Symbol) => updated(it)
              case it => it
            }
          it.cc(params = paramsʹ)

        case it => it

      }
