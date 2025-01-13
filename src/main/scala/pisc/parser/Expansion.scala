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

package scala.util.parsing.combinator
package pisc
package parser

import java.util.regex.Pattern
import scala.util.matching.Regex
import Regex.Match

import scala.collection.mutable.{ LinkedHashMap => Map, LinkedHashSet => Set }

import scala.meta.Term

import _root_.pisc.parser.Expression
import Expression.Code
import _root_.pisc.parser.{ Encoding, & }
import _root_.pisc.parser.PolyadicPi._
import _root_.pisc.parser.Calculus._
import Encoding._
import Expansion._


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

  def instance(defs: List[Define], end: String)
              (using Names2): Parser[(`⟦⟧`, Names)] =
    var idx = -1

    new Parser[(`⟦⟧`, Names)] {

      def expand(in: Input, shadows: List[Option[Symbol]], key: CacheKey)
                (op: String, end: Either[String, String])
                (success: Input => (ParseResult[Fresh], Seq[Term]))
                (using binding2: Names2)
                (using substitution: Map[String, λ | AST])
                (using free: Names): (ParseResult[Fresh], Seq[Term]) =

        val source = in.source
        val offset = in.offset
        val start = handleWhiteSpace(source, offset)

        if (open_r findPrefixMatchOf SubSequence(source, start, 1 min (source.length - start))).nonEmpty
        then

          if op.charAt(0).isLower || op == "_"
          then
            Failure("name expected not instantiation", in) -> Nil

          else

            _cache.get(key) match

              case Some((exp: `⟦⟧`, cp, free2, given Names2, in2)) =>
                binding2 ++= binders

                substitution(op) = exp
                free ++= free2 -- binding2.map(_._1)

                paste(cp)

                success(in2)

              case _ =>

                given Names2 = Names2(binding2)
                parse(instantiation, in) match

                  case Success((exp, free2), in) =>
                    binding2 ++= binders

                    substitution(op) = exp
                    free ++= free2 -- binding2.map(_._1)

                    val source = in.source
                    val offset = in.offset
                    val start = handleWhiteSpace(source, offset)

                    val in2 = in.drop(start + end.map(_.length).getOrElse(0) - offset)

                    _cache(key) = (exp, copy, free2, given_Names2, in2)

                    success(in2)

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

              if op.charAt(0).isUpper
              then
                Failure("choice expected", in) -> Nil

              else
                val in2 = in.drop(start + n - offset)
                success(in2)

            else if op.charAt(0).isLower || op == "_"
            then

              parseAll(name, result) match

                case Success((λ(it: Symbol), free2), _) =>
                  shadows(idx) match
                    case shadow @ Some(_) =>
                      Names2Occurrence(it, shadow)
                    case _ =>
                      binding2.find { case (`it`, Shadow(_)) => true case _ => false } match
                        case Some((_, Shadow(it))) => substitution(op) = λ(it)
                        case _ => substitution(op) = λ(it)
                      free ++= free2 -- binding2.map(_._1)
                  idx += 1

                  val in2 = in.drop(start + n - offset)
                  success(in2)

                case _ =>
                  Failure("name expected", in) -> Nil

            else

              _cache.get(key) match

                case Some((sum: +, cp, free2, given Names2, in2)) =>
                  binding2 ++= binders

                  substitution(op) = sum
                  free ++= free2 -- binding2.map(_._1)

                  paste(cp)

                  success(in2)

                case _ =>

                  given Names2 = Names2(binding2)
                  parseAll(choice, result) match

                    case Success((sum, free2), _) =>
                      binding2 ++= binders

                      val sum2 = sum.flatten.update(using Names2(binding2))

                      substitution(op) = sum2
                      free ++= free2 -- binding2.map(_._1)

                      val in2 = in.drop(start + n - offset)

                      _cache(key) = (sum2, copy, free2, given_Names2, in2)

                      success(in2)

                    case _ =>
                      Failure("choice expected", in) -> Nil

          case _ =>
            val found = if start == source.length
                        then "end of source"
                        else "'"+source.charAt(start)+"'"
            Failure(end.map("operator '" + _ + "'").orElse(end.swap).right.get+" expected but "+found+" found", in.drop(start - offset)) -> Nil


      def expand(in: Input, _ts: Seq[Term], end: Either[String, String])
                (using Names2)
                (using Map[String, λ | AST])
                (using Names): ((Fresh, Term)) => (ParseResult[Fresh], Seq[Term]) =

        case (it @ (_, (_, shadows)), _rhs @ (Term.Name(_) | Term.Placeholder())) =>
          val rhs = _rhs match { case Term.Name(rhs) => rhs case Term.Placeholder() => "_" }

          val source = in.source
          val offset = in.offset
          val start = handleWhiteSpace(source, offset)

          val ts = _ts :+ _rhs
          val key = path -> (ts.mkString -> end) -> start

          def success(in2: Input) =
            Success(it, in2) -> ts

          expand(in, shadows, key)(rhs, end)(success)

        case (it @ (_, (_, shadows)), Term.ApplyInfix(_lhs @ (Term.Name(_) | Term.Placeholder()), _op @ Term.Name(op), _, List(rhs))) =>
          val lhs = _lhs match { case Term.Name(lhs) => lhs case Term.Placeholder() => "_" }

          val source = in.source
          val offset = in.offset
          val start = handleWhiteSpace(source, offset)

          val ts = _ts :+ _lhs :+ _op
          val key = path -> (ts.mkString -> end) -> start

          def success(in2: Input) =
            expand(in2, ts, end)(it -> rhs)

          expand(in, shadows, key)(lhs, Right(op))(success)

        case (it, Term.ApplyInfix(lhs: Term.ApplyInfix, _op @ Term.Name(op), _, List(rhs))) =>
          expand(in, _ts, Right(op))(it -> lhs) match
            case (Success(_, in2), ts) =>
              expand(in2, ts :+ _op, end)(it -> rhs)
            case it => it

        case (it, Term.AnonymousFunction(body)) =>
          expand(in, _ts, end)(it -> body)

        case _ => ??? /* caught by template */

      override def apply(in: Input): ParseResult[(`⟦⟧`, Names)] =
        val binding2 = summon[Names2]

        var r: Option[(Fresh, (Map[String, λ | AST], (Names, Names2)), Input)] = None

        var ls = defs
        while ls.nonEmpty
        do
          val (_macro, Definition(code, Some(term), _, _, _)) = ls.head : @unchecked
          ls = ls.tail

          given Map[String, λ | AST]()
          given Names()
          given Names2 = Names2(binding2)
          idx = 0

          save(expand(in, Nil, Left(end))(_macro(code, id, term) -> term), ls.isEmpty && r.isEmpty) match
            case Some(_) if r.nonEmpty => throw AmbiguousParsingException
            case Some((it @ (_, (arity, _)), in)) if arity == given_Map_String_|.size =>
              r = Some((it, given_Map_String_| -> (given_Names -> given_Names2), in))
            case _ =>

        r match

          case Some(((definition, _), (given Map[String, λ | AST], (free, given Names2)), in)) =>
            binding2 ++= binders

            Success(definition() -> free, in)

          case _ => throw UndefinedParsingException

    }.named(s"""⟦${if defs.size == 1 then defs.head._2.code.toString else ""}⟧""")


object Expansion:

  private val open_r = """⟦\d*""".r
  private val closed_r = """\d*⟧""".r

  // exceptions

  import _root_.pisc.parser.Expression.ParsingException

  case object AmbiguousParsingException
      extends ParsingException(s"An instantiation of a template is ambiguous")

  case object UndefinedParsingException
      extends ParsingException(s"An instantiation of a template is undefined")


  // functions

  private[parser] def balanced(end: Either[String, String])
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
              (using substitution: Map[String, λ | AST]): λ =
    substitution.get(name.name) match
      case Some(it: λ) => it
      case _ => λ(name)

  def updated(name: Symbol)
             (using binding2: Names2): λ =
    binding2.find { case (`name`, Shadow(_)) => true case _ => false } match
      case Some((_, Shadow(it))) => λ(it)
      case _ => λ(name)

  private def recoded(using code: Option[Code])
                     (using substitution: Map[String, λ | AST] = null)
                     (using updating: Names2 = null): Option[Code] =
    code.map { (_, orig) =>
      Expression(orig)._1 match
        case term @ Term.ForYield(enums, _) =>
          (Left(enums), term)
        case term =>
          (Right(term), term)
    }


  extension[T <: AST](ast: T)

    def replace(using substitution: Map[String, λ | AST]): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅ => ∅

        case +(it*) =>
          `+`(it.map(_.replace)*)

        case ∥(it*) =>
          ∥(it.map(_.replace)*)

        case `.`(end, _it*) =>
          val it = _it.map {
            case it @ τ(given Option[Code]) =>
              it.copy(code = recoded)
            case π(λ(ch: Symbol), true, given Option[Code], names*) =>
              π(replaced(ch), true, recoded, names*)
            case π(λ(ch: Symbol), false, given Option[Code], _names*) =>
              val names = _names.map {
                case λ(arg: Symbol) => replaced(arg)
                case it => it
              }
              π(replaced(ch), false, recoded, names*)
            case it => it
          }
          `.`(end.replace, it*)

        case ?:(((λ(lhs: Symbol), λ(rhs: Symbol)), m), t, f) =>
          ?:(((replaced(lhs), replaced(rhs)), m), t.replace, f.map(_.replace))

        case ?:(((λ(lhs: Symbol), rhs), m), t, f) =>
          ?:(((replaced(lhs), rhs), m), t.replace, f.map(_.replace))

        case ?:(((lhs, λ(rhs: Symbol)), m), t, f) =>
          ?:(((lhs, replaced(rhs)), m), t.replace, f.map(_.replace))

        case ?:(cond, t, f) =>
          ?:(cond, t.replace, f.map(_.replace))

        case !(Some(it @ τ(given Option[Code])), sum) =>
          `!`(Some(it.copy(code = recoded)), sum.replace)

        case !(Some(π(λ(ch: Symbol), true, given Option[Code], names*)), sum) =>
          `!`(Some(π(replaced(ch), true, recoded, names*)), sum.replace)

        case !(Some(π(λ(ch: Symbol), false, given Option[Code], _names*)), sum) =>
          val names = _names.map {
            case λ(arg: Symbol) => replaced(arg)
            case it => it
          }
          `!`(Some(π(replaced(ch), false, recoded, names*)), sum.replace)

        case it @ !(_, sum) =>
          it.copy(sum = sum.replace)

        case it @ `⟦⟧`(_, _, sum, _) =>
          val assign = it.assign.map(_.map(_ -> replaced(_).asSymbol))
          it.copy(sum = sum.replace, assign = assign)

        case `{}`(id, pointers, false) =>
          given List[Symbol] = pointers.map(replaced(_).asSymbol)
          if given_List_Symbol.nonEmpty
          then
            substitution(id).asInstanceOf[&].flatten.concatenate
          else
            substitution(id).asInstanceOf[&]

        case `{}`(id, pointers, true, params*) =>
          val pointers2 = pointers.map(replaced(_).asSymbol)
          val params2 = params
            .map {
              case λ(it: Symbol) => replaced(it)
              case it => it
            }

          `{}`(id, pointers2, true, params2*)

        case _: `{}` => ???

        case `(*)`(id, qual, params*) =>
          val params2 = params
            .map {
              case λ(it: Symbol) => replaced(it)
              case it => it
            }

          `(*)`(id, qual, params2*)


    private def concatenate(using pointers: List[Symbol]): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅ => ∅

        case +(it*) =>
          `+`(it.map(_.concatenate)*)

        case ∥(it*) =>
          ∥(it.map(_.concatenate)*)

        case `.`(end, it*) =>
          `.`(end.concatenate, it*)

        case ?:(cond, t, f) =>
          ?:(cond, t.concatenate, f.map(_.concatenate))

        case it @ !(_, sum) =>
          it.copy(sum = sum.concatenate)

        case it @ `⟦⟧`(_, variables, _, _) =>
          val n = it.assign.map(_.size).getOrElse(0)
          val assign = variables.drop(n) zip pointers
          val assign2 = it.assign.getOrElse(Set.empty) ++ assign
          it.copy(assign = if assign2.nonEmpty then Some(assign2) else None)

        case it @ `{}`(id, _, agent, params*) =>
          `{}`(id, it.pointers ++ pointers, agent, params*)

        case it => it


    def update(using binding2: Names2): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅ => ∅

        case +(it*) =>
          `+`(it.map(_.update)*)

        case ∥(it*) =>
          ∥(it.map(_.update)*)

        case `.`(end, _it*) =>
          given Names2 = Names2(binding2)
          val it = _it.map {
            case it @ ν(names*) =>
              given_Names2 --= names.map(_._2).map(Symbol(_))
              it
            case it @ τ(given Option[Code]) =>
              it.copy(code = recoded)
            case π(λ(ch: Symbol), true, given Option[Code], names*) =>
              val ch2 = updated(ch)
              given_Names2 --= names.map(_.asSymbol)
              π(ch2, true, recoded, names*)
            case π(λ(ch: Symbol), false, given Option[Code], _names*) =>
              val names = _names.map {
                case λ(arg: Symbol) => updated(arg)
                case it => it
              }
              π(updated(ch), false, recoded, names*)
            case it => it
          }
          `.`(end.update, it*)

        case ?:(((λ(lhs: Symbol), λ(rhs: Symbol)), m), t, f) =>
          ?:(((updated(lhs), updated(rhs)), m), t.update, f.map(_.update))

        case ?:(((λ(lhs: Symbol), rhs), m), t, f) =>
          ?:(((updated(lhs), rhs), m), t.update, f.map(_.update))

        case ?:(((lhs, λ(rhs: Symbol)), m), t, f) =>
          ?:(((lhs, updated(rhs)), m), t.update, f.map(_.update))

        case ?:(cond, t, f) =>
          ?:(cond, t.update, f.map(_.update))

        case !(Some(it @ τ(given Option[Code])), sum) =>
          `!`(Some(it.copy(code = recoded)), sum.update)

        case !(Some(π(λ(ch: Symbol), true, given Option[Code], names*)), sum) =>
          given Names2 = Names2(binding2)
          val ch2 = updated(ch)
          given_Names2 --= names.map(_.asSymbol)
          `!`(Some(π(ch2, true, recoded, names*)), sum.update)

        case !(Some(π(λ(ch: Symbol), false, given Option[Code], _names*)), sum) =>
          val names = _names.map {
            case λ(arg: Symbol) => updated(arg)
            case it => it
          }
          `!`(Some(π(updated(ch), false, recoded, names*)), sum.update)

        case it @ !(_, sum) =>
          it.copy(sum = sum.update)

        case it @ `⟦⟧`(_, _, sum, assign) =>
          val assign2 = assign.map(_.map(_ -> updated(_).asSymbol))
          it.copy(sum = sum.update, assign = assign2)

        case `{}`(id, pointers, agent, params*) =>
          val pointers2 = pointers.map(updated(_).asSymbol)
          val params2 = params
            .map {
              case λ(it: Symbol) => updated(it)
              case it => it
            }

          `{}`(id, pointers2, agent, params2*)

        case `(*)`(id, qual, params*) =>
          val params2 = params
            .map {
              case λ(it: Symbol) => updated(it)
              case it => it
            }

          `(*)`(id, qual, params2*)
