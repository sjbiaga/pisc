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

package scala.util.parsing.combinator
package pisc
package parser

import java.util.regex.Pattern
import scala.util.matching.Regex

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

  private val open = """⟦\d*""".r
  private val closed = """\d*⟧""".r

  def instance(defs: List[Define], end: String)
              (using Names2): Parser[(`⟦⟧`, Names)] =
    var idx = -1

    new Parser[(`⟦⟧`, Names)] {

      private def expand(in: Input, end: Either[String, String])
                        (using binding2: Names2)
                        (using substitution: Map[String, λ | AST])
                        (using free: Names): ((Fresh, Term)) => ParseResult[Fresh] =

        case (it @ (_, shadows), _rhs @ (Term.Name(_) | Term.Placeholder())) =>
          val rhs = _rhs match { case Term.Name(rhs) => rhs case Term.Placeholder() => "_" }

          var source = in.source
          var offset = in.offset
          var start = handleWhiteSpace(source, offset)

          var s = SubSequence(source, start)

          if (open findPrefixMatchOf s).nonEmpty
          then

            if rhs.charAt(0).isLower || rhs == "_"
            then
              Failure("name expected not instantiation", in)

            else

              parse(instantiation, in) match

                case Success((exp, free2), in) =>
                  substitution(rhs) = exp
                  free ++= free2 -- binding2.map(_._1)

                  source = in.source
                  offset = in.offset
                  start = handleWhiteSpace(source, offset)

                  Success(it, in.drop(start + end.map(_.length).getOrElse(0) - offset))

                case it =>
                  Failure("instantiation expected", it.next)

          else
            ( if s.isEmpty
              || s.charAt(0) == '_' && (s.length == 1 || s.charAt(1).isWhitespace)
              || (closed findPrefixMatchOf s).nonEmpty
              then Some(null)
              else
                val op = end.orElse(end.swap).right.get
                val n = op.length
                val matches = (Pattern.quote(op).r findAllMatchIn s).toList
                var i = 0
                while i < matches.size
                && { s = SubSequence(source, start, matches(i).end - n); true }
                && (open findAllMatchIn s).size > (closed findAllMatchIn s).size
                do
                  i += 1
                if i < matches.size then Some(matches(i)) else None
            ) match

              case Some(matched) =>
                var n = ( if matched eq null
                          then
                            if !s.isEmpty && s.charAt(0) == '_' && (s.length == 1 || s.charAt(1).isWhitespace)
                            then 1
                            else 0
                          else matched.end - end.orElse(end.swap).map(_.length).right.get
                        )
                val result = source.subSequence(start, start + n).toString

                var fail: Option[ParseResult[Fresh]] = None

                if result.isBlank
                || result.strip == "_"
                then
                  if matched eq null
                  then
                    n += result.length
                  else
                    n += end.map(_.length).getOrElse(0)
                else if (closed findPrefixMatchOf s).nonEmpty
                then
                  {}
                else
                  n += end.map(_.length).getOrElse(0)

                  if rhs.charAt(0).isLower || rhs == "_"
                  then

                    parseAll(name, result) match
                      case Success((λ(it: Symbol), free2), _) =>
                        shadows(idx) match
                          case shadow @ Some(_) =>
                            Names2(it, shadow)
                          case _ =>
                            binding2.find { case (`it`, Shadow(_)) => true case _ => false } match
                              case Some((_, Shadow(it))) => substitution(rhs) = λ(it)
                              case _ => substitution(rhs) = λ(it)
                            free ++= free2 -- binding2.map(_._1)
                        idx += 1

                      case it => fail = Some(Failure("name expected", it.next))

                  else
                    given Names2 = Names2(binding2)
                    parseAll(choice, result) match
                      case Success((sum, free2), _) =>
                        Expression.renaming = None
                        Expression.replacing = None
                        given Names2 = Names2(binding2)
                        substitution(rhs) = sum.flatten.update
                        free ++= free2 -- binding2.map(_._1)

                      case it => fail = Some(Failure("choice expected", it.next))

                fail match
                  case Some(it) => it
                  case _ =>
                    Success(it, in.drop(start + n - offset))

              case _ =>
                val found = if start == source.length
                            then "end of source"
                            else "'"+source.charAt(start)+"'"
                Failure(end.orElse(end.swap).right.get+" expected but "+found+" found", in.drop(start - offset))

        case (it @ (_, shadows), Term.ApplyInfix(_lhs @ (Term.Name(_) | Term.Placeholder()), Term.Name(op), _, List(rhs))) =>
          val lhs = _lhs match { case Term.Name(lhs) => lhs case Term.Placeholder() => "_" }

          var source = in.source
          var offset = in.offset
          var start = handleWhiteSpace(source, offset)

          var s = SubSequence(source, start)

          if (open findPrefixMatchOf s).nonEmpty
          then

            if lhs.charAt(0).isLower || lhs == "_"
            then
              Failure("name expected not instantiation", in)

            else

              parse(instantiation, in) match

                case Success((exp, free2), in) =>
                  substitution(lhs) = exp
                  free ++= free2 -- binding2.map(_._1)

                  source = in.source
                  offset = in.offset
                  start = handleWhiteSpace(source, offset)

                  expand(in.drop(start + op.length - offset), end)(it -> rhs)

                case it =>
                  Failure("instantiation expected", it.next)

          else {
            val n = op.length
            val matches = (Pattern.quote(op).r findAllMatchIn s).toList
            var i = 0
            while i < matches.size
            && { s = SubSequence(source, start, matches(i).end - n); true }
            && (open findAllMatchIn s).size > (closed findAllMatchIn s).size
            do
              i += 1
            if i < matches.size then Some(matches(i)) else None
          } match

            case Some(matched) =>
              val result = source.subSequence(start, start + matched.end - op.length).toString

              var fail: Option[ParseResult[Fresh]] = None

              if result.isBlank
              || result.strip == "_"
              then
                {}
              else if lhs.charAt(0).isLower || lhs == "_"
              then
                parseAll(name, result) match
                  case Success((λ(it: Symbol), free2), _) =>
                    shadows(idx) match
                      case shadow @ Some(_) =>
                        Names2(it, shadow)
                      case _ =>
                        binding2.find { case (`it`, Shadow(_)) => true case _ => false } match
                          case Some((_, Shadow(it))) => substitution(lhs) = λ(it)
                          case _ => substitution(lhs) = λ(it)
                        free ++= free2 -- binding2.map(_._1)
                    idx += 1

                  case it => fail = Some(Failure("name expected", it.next))

              else
                given Names2 = Names2(binding2)
                parseAll(choice, result) match
                  case Success((sum, free2), _) =>
                    Expression.renaming = None
                    Expression.replacing = None
                    given Names2 = Names2(binding2)
                    substitution(lhs) = sum.flatten.update
                    free ++= free2 -- binding2.map(_._1)

                  case it => fail = Some(Failure("choice expected", it.next))

              fail match
                case Some(it) => it
                case _ =>
                  expand(in.drop(start + matched.end - offset), end)(it -> rhs)

            case _ =>
              val found = if start == source.length
                          then "end of source"
                          else "'"+source.charAt(start)+"'"
              Failure("operator '"+op+"' expected but "+found+" found", in.drop(start - offset))

        case (it, Term.ApplyInfix(lhs: Term.ApplyInfix, Term.Name(op), _, List(rhs))) =>
          expand(in, Right(op))(it -> lhs) match

            case Success(_, _in) =>
              var in = _in
              var source = in.source
              var offset = in.offset
              var start = handleWhiteSpace(source, offset)

              var s = SubSequence(source, start)

              var placeholder = false

              if s.charAt(0) == '_' && (s.length == 1 || s.charAt(1).isWhitespace)
              then
                in = in.drop(start + 1 - offset)
                source = in.source
                offset = in.offset
                start = handleWhiteSpace(source, offset)

                s = SubSequence(source, start)

                placeholder = true

              (Pattern.quote(end.orElse(end.swap).right.get).r findPrefixMatchOf s) match

                case Some(matched) =>
                  Success(it, in.drop(start + matched.end - offset))

                case _ if placeholder =>
                  Failure(s"${end.orElse(end.swap).right.get} expected", in)

                case _ =>
                  expand(in, end)(it -> rhs)

            case it => it

        case (it, Term.AnonymousFunction(body)) =>
          expand(in, end)(it -> body)

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

          save(expand(in, Left(end))(_macro(code, term) -> term), ls.isEmpty && r.isEmpty) match
            case Some(_) if r.nonEmpty => throw AmbiguousParsingException
            case Some((it, in)) => r = Some((it, given_Map_String_| -> (given_Names -> given_Names2), in))
            case _ =>

        r match
          case Some(((definition, _), (given Map[String, λ | AST], (free, given Names2)), in)) =>
            binding2 ++= given_Names2.filter(_._2.isBinding < 0)
            Expression.renaming = None
            Expression.updating = None
            Expression.replacing = Some(given_Map_String_|)
            Success(definition() -> free, in)
          case _ => throw UndefinedParsingException

    }.named(s"""⟦${if defs.size == 1 then defs.head._2.code.toString else ""}⟧""")


object Expansion:

  // exceptions

  import _root_.pisc.parser.Expression.ParsingException

  case object AmbiguousParsingException
      extends ParsingException(s"An instantiation of a template is ambiguous")

  case object UndefinedParsingException
      extends ParsingException(s"An instantiation of a template is undefined")


  // functions

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

  private def recoded(using code: Option[Code]): Option[Code] =
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

        case ||(it*) =>
          ||(it.map(_.replace)*)

        case `.`(end, _it*) =>
          val it = _it.map {
            case it @ τ(given Option[Code]) =>
              it.copy(code = recoded)
            case π(λ(ch: Symbol), true, code, names*) =>
              given Option[Code] = code
              π(replaced(ch), true, recoded, names*)
            case π(λ(ch: Symbol), false, code, _names*) =>
              given Option[Code] = code
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

        case !(Some(π(λ(ch: Symbol), true, code, names*)), sum) =>
          given Option[Code] = code
          `!`(Some(π(replaced(ch), true, recoded, names*)), sum.replace)

        case !(Some(π(λ(ch: Symbol), false, code, _names*)), sum) =>
          given Option[Code] = code
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

        case ||(it*) =>
          ||(it.map(_.concatenate)*)

        case `.`(end, it*) =>
          `.`(end.concatenate, it*)

        case ?:(cond, t, f) =>
          ?:(cond, t.concatenate, f.map(_.concatenate))

        case it @ !(_, sum) =>
          it.copy(sum = sum.concatenate)

        case it @ `⟦⟧`(_, variables, _, _) =>
          val n = it.assign.map(_.size).getOrElse(0)
          val assign = variables.drop(n) zip pointers
          val assign2 = it.assign.map(_ ++ assign).getOrElse(assign)
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

        case ||(it*) =>
          ||(it.map(_.update)*)

        case `.`(end, _it*) =>
          given Names2 = Names2(binding2)
          Expression.updating = Some(given_Names2)
          val it = _it.map {
            case it @ ν(names*) =>
              given_Names2 --= names.map(_._2).map(Symbol(_))
              it
            case it @ τ(given Option[Code]) =>
              it.copy(code = recoded)
            case π(λ(ch: Symbol), true, code, names*) =>
              given Option[Code] = code
              val ch2 = updated(ch)
              given_Names2 --= names.map(_.asSymbol)
              π(ch2, true, recoded, names*)
            case π(λ(ch: Symbol), false, code, _names*) =>
              given Option[Code] = code
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
          given Names2 = Names2(binding2)
          Expression.updating = Some(given_Names2)
          `!`(Some(it.copy(code = recoded)), sum.update)

        case !(Some(π(λ(ch: Symbol), true, code, names*)), sum) =>
          given Option[Code] = code
          given Names2 = Names2(binding2)
          Expression.updating = Some(given_Names2)
          val ch2 = updated(ch)
          given_Names2 --= names.map(_.asSymbol)
          `!`(Some(π(ch2, true, recoded, names*)), sum.update)

        case !(Some(π(λ(ch: Symbol), false, code, _names*)), sum) =>
          given Option[Code] = code
          given Names2 = Names2(binding2)
          Expression.updating = Some(given_Names2)
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
