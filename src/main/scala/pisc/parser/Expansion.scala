/*
 * Copyright (c) 2023-2024 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell coPolyadicPies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all coPolyadicPies or substantial portions of the Software.
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

import _root_.pisc.parser.{ Calculus, & }
import _root_.pisc.parser.PolyadicPi._
import Calculus._
import Expansion._


abstract class Expansion extends Calculus:

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

  def instance(defs: List[Encoding], end: String)
              (using Names2): Parser[(`⟦⟧`, Names)] =
    var idx = -1

    new Parser[(`⟦⟧`, Names)] {

      private def expand(in: Input, end: Either[String, String])
                        (using binding2: Names2)
                        (using substitution: Map[String, λ | AST])
                        (using free: Names): (((Encoding, +), Term)) => ParseResult[(Encoding, +)] =

        case (it @ (encoding, _), _rhs @ (Term.Name(_) | Term.Placeholder())) =>
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

                var fail: Option[ParseResult[(Encoding, +)]] = None

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
                      case Success((λ(name: Symbol), free2), _) =>
                        encoding.shadows(idx) match
                          case shadow @ Some(_) =>
                            Names2(name, shadow)
                          case _ =>
                            binding2.find { case (`name`, Shadow(_)) => true case _ => false } match
                              case Some((_, Shadow(it))) => substitution(rhs) = λ(it)
                              case _ => substitution(rhs) = λ(name)
                            free ++= free2 -- binding2.map(_._1)
                        idx += 1

                      case it => fail = Some(Failure("name expected", it.next))

                  else
                    parseAll(choice, result) match
                      case Success((sum, free2), _) =>
                        substitution(rhs) = sum
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

        case (it @ (encoding, _), Term.ApplyInfix(_lhs @ (Term.Name(_) | Term.Placeholder()), Term.Name(op), _, List(rhs))) =>
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

              var fail: Option[ParseResult[(Encoding, +)]] = None

              if result.isBlank
              || result.strip == "_"
              then
                {}
              else if lhs.charAt(0).isLower || lhs == "_"
              then
                parseAll(name, result) match
                  case Success((λ(name: Symbol), free2), _) =>
                    encoding.shadows(idx) match
                      case shadow @ Some(_) =>
                        Names2(name, shadow)
                      case _ =>
                        binding2.find { case (`name`, Shadow(_)) => true case _ => false } match
                          case Some((_, Shadow(it))) => substitution(lhs) = λ(it)
                          case _ => substitution(lhs) = λ(name)
                        free ++= free2 -- binding2.map(_._1)
                    idx += 1

                  case it => fail = Some(Failure("name expected", it.next))

              else
                parseAll(choice, result) match
                  case Success((sum, free2), _) =>
                    substitution(lhs) = sum
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

        case (it @ (encoding, _), Term.ApplyInfix(lhs: Term.ApplyInfix, Term.Name(op), _, List(rhs))) =>
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

        var r: Option[((Encoding, +), (Map[String, λ | AST], (Names, Names2)), Input)] = None

        var ls = defs
        while ls.nonEmpty
        do
          val Encoding(code, Some(term), Some(defn), _, _, _) = ls.head : @unchecked
          ls = ls.tail

          given Map[String, λ | AST]()
          given Names()
          given Names2 = Names2(binding2)
          idx = 0

          save(expand(in, Left(end))(defn(code, term) -> term), ls.isEmpty && r.isEmpty) match
            case Some(_) if r.nonEmpty => throw AmbiguousParsingException
            case Some((it, in)) => r = Some((it, given_Map_String_| -> (given_Names -> given_Names2), in))
            case _ =>

        r match
          case Some(((encoding, sum), (given Map[String, λ | AST], (free, given Names2)), in)) =>
            binding2 ++= given_Names2.filter(_._2.isBinding < 0)
            Success(`⟦⟧`(encoding, sum.replace.flatten) -> free, in)
          case _ => throw UndefinedParsingException

    }.named(s"""⟦${if defs.size == 1 then defs.head.code.toString else ""}⟧""")


object Expansion:

  // exceptions

  import _root_.pisc.parser.Expression.ParsingException

  case object AmbiguousParsingException
      extends ParsingException(s"An instantiation of a template is ambiguous")

  case object UndefinedParsingException
      extends ParsingException(s"An instantiation of a template is undefined")


  // functions

  private def replaced(name: Symbol)
                      (using substitution: Map[String, λ | AST]): λ =
    substitution.get(name.name) match
      case Some(it: λ) => it
      case _ => λ(name)


  extension[T <: AST](ast: T)

    def replace(using substitution: Map[String, λ | AST])
               (using pointers: Option[List[Symbol]] = None): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅ => ∅

        case +(it*) =>
          `+`(it.map(_.replace)*)

        case ||(it*) =>
          ||(it.map(_.replace)*)

        case `.`(end, _it*) =>
          val it = _it.map {
            case π(λ(ch: Symbol), true, code, _names*) =>
              val names = _names.map(_.asSymbol).map(replaced(_))
              π(replaced(ch), true, code, names*)
            case π(λ(ch: Symbol), false, code, _names*) =>
              val names = _names.map {
                case λ(arg: Symbol) => replaced(arg)
                case it => it
              }
              π(replaced(ch), false, code, names*)
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

        case !(Some(π(λ(ch: Symbol), true, code, _names*)), sum) =>
          val names = _names.map(_.asSymbol).map(replaced(_))
          `!`(Some(π(replaced(ch), true, code, names*)), sum.replace)

        case !(Some(π(λ(ch: Symbol), false, code, _names*)), sum) =>
          val names = _names.map {
            case λ(arg: Symbol) => replaced(arg)
            case it => it
          }
          `!`(Some(π(replaced(ch), false, code, names*)), sum.replace)

        case it @ !(_, sum) =>
          it.copy(sum = sum.replace)

        case it @ `⟦⟧`(_, sum, _assign) if pointers.isEmpty =>
          val assign = _assign.map(_.map(_ -> replaced(_).asSymbol))
          it.copy(sum = sum.replace, assign = assign)

        case it @ `⟦⟧`(Encoding(_, _, _, _, _, variables), _, assign0) =>
          val n = assign0.map(_.size).getOrElse(0)
          val assign1 = variables.drop(n) zip pointers.get.drop(n)
          val assign = assign0.map(_ ++ assign1).getOrElse(assign1)
          it.copy(assign = Some(assign))

        case `{}`(id, pointers) if substitution.contains(id) =>
          val pointers2 = pointers.map(replaced(_).asSymbol)
          substitution(id).asInstanceOf[&].replace(using Map.empty)(using Some(pointers2))

        case `{}`(id, pointers0) =>
          `{}`(id, pointers0 ++ pointers.get)

        case `(*)`(id, qual, params*) =>
          val params2 = params
            .map {
              case λ(it: Symbol) => replaced(it)
              case it => it
            }

          `(*)`(id, qual, params2*)
