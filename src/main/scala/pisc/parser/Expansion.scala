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

import _root_.pisc.parser.{ Calculus, & }
import _root_.pisc.parser.Pi._
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

  def instance(defs: List[Define], end: String)
              (using Names2): Parser[(`⟦⟧`, Names)] =
    var idx = -1

    new Parser[(`⟦⟧`, Names)] {

      private def expand(in: Input, end: Either[String, String])
                        (using binding2: Names2)
                        (using substitution: Map[String, λ | AST])
                        (using free: Names): ((Define, Term)) => ParseResult[Unit] =

        case ((encoding, sum), Term.Name(rhs)) =>
          var source = in.source
          var offset = in.offset
          var start = handleWhiteSpace(source, offset)

          var s = SubSequence(source, start)

          if ("""⟦\d*""".r findPrefixMatchOf s).nonEmpty
          then

            if rhs.charAt(0).isLower || rhs.charAt(0) == '_'
            then
              Failure("name expected not instantiation", in)

            else

              parse(instantiation, in) match

                case Success((rhs2, free2), in) =>
                  substitution(rhs) = rhs2
                  free ++= free2 -- binding2.map(_._1)

                  source = in.source
                  offset = in.offset
                  start = handleWhiteSpace(source, offset)

                  Success((), in.drop(start + end.map(_.length).getOrElse(0) - offset))

                case it =>
                  Failure("instantiation expected", it.next)

          else
            ( if s.isEmpty
              || s.charAt(0) == '_' && (s.length == 1 || s.charAt(1).isWhitespace)
              || ("""\d*⟧""".r findPrefixMatchOf s).nonEmpty
              then Some(null)
              else
                val op = end.orElse(end.swap).right.get
                val n = op.length
                val open = """⟦\d*""".r
                val closed = """\d*⟧""".r
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

                var fail: Option[ParseResult[Unit]] = None

                if result.isBlank
                || result.strip == "_"
                then
                  n += result.length
                else if ("""\d*⟧""".r findPrefixMatchOf s).nonEmpty
                then
                  {}
                else
                  n += end.map(_.length).getOrElse(0)

                  if rhs.charAt(0).isLower || rhs.charAt(0) == '_'
                  then

                    parseAll(name, result) match
                      case Success((λ(name2: Symbol), free2), _) =>
                        encoding.shadows(idx) match
                          case shadow @ Some(_) =>
                            Names2(name2, shadow)
                          case _ =>
                            binding2.find { case (`name2`, Shadow(_)) => true case _ => false } match
                              case Some((_, Shadow(it))) => substitution(rhs) = λ(it)
                              case _ => substitution(rhs) = λ(name2)
                            free ++= free2 -- binding2.map(_._1)
                        idx += 1

                      case it => fail = Some(Failure("name expected", it.next))

                  else
                    parseAll(choice, result) match
                      case Success((sum2, free2), _) =>
                        substitution(rhs) = sum2
                        free ++= free2 -- binding2.map(_._1)

                      case it => fail = Some(Failure("choice expected", it.next))

                fail match
                  case Some(it) => it
                  case _ =>
                    Success((), in.drop(start + n - offset))

              case _ =>
                val found = if start == source.length
                            then "end of source"
                            else "'"+source.charAt(start)+"'"
                Failure(end.orElse(end.swap).right.get+" expected but "+found+" found", in.drop(start - offset))

        case (it @ (encoding, sum), Term.ApplyInfix(Term.Name(lhs), Term.Name(op), _, List(rhs))) =>
          var source = in.source
          var offset = in.offset
          var start = handleWhiteSpace(source, offset)

          var s = SubSequence(source, start)

          if ("""⟦\d*""".r findPrefixMatchOf s).nonEmpty
          then

            if lhs.charAt(0).isLower || lhs.charAt(0) == '_'
            then
              Failure("name expected not instantiation", in)

            else

              parse(instantiation, in) match

                case Success((lhs2, free2), in) =>
                  substitution(lhs) = lhs2
                  free ++= free2 -- binding2.map(_._1)

                  source = in.source
                  offset = in.offset
                  start = handleWhiteSpace(source, offset)

                  expand(in.drop(start + op.length - offset), end)(it -> rhs)

                case it =>
                  Failure("instantiation expected", it.next)

          else {
            val n = op.length
            val open = """⟦\d*""".r
            val closed = """\d*⟧""".r
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

              var fail: Option[ParseResult[Unit]] = None

              if result.isBlank
              || result.strip == "_"
              then
                {}
              else if lhs.charAt(0).isLower || lhs.charAt(0) == '_'
              then
                parseAll(name, result) match
                  case Success((λ(name2: Symbol), free2), _) =>
                    encoding.shadows(idx) match
                      case shadow @ Some(_) =>
                        Names2(name2, shadow)
                      case _ =>
                        binding2.find { case (`name2`, Shadow(_)) => true case _ => false } match
                          case Some((_, Shadow(it))) => substitution(lhs) = λ(it)
                          case _ => substitution(lhs) = λ(name2)
                        free ++= free2 -- binding2.map(_._1)
                    idx += 1

                  case it => fail = Some(Failure("name expected", it.next))

              else
                parseAll(choice, result) match
                  case Success((sum2, free2), _) =>
                    substitution(lhs) = sum2
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

        case (it @ (encoding, sum), Term.ApplyInfix(lhs: Term.ApplyInfix, Term.Name(op), _, List(rhs))) =>
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
                  Success((), in.drop(start + matched.end - offset))

                case _ if placeholder =>
                  Failure(s"${end.orElse(end.swap).right.get} expected", in)

                case _ =>
                  expand(in, end)(it -> rhs)

            case it => it

        case _ => ??? /* caught by template */

      override def apply(in: Input): ParseResult[(`⟦⟧`, Names)] =
        val binding2 = summon[Names2]

        var r: Option[(Define, (Map[String, λ | AST], Names), Input)] = None

        var ls = defs
        while ls.nonEmpty
        do
          val it = ls.head
          val term = it._1.term.get
          ls = ls.tail

          given Map[String, λ | AST]()
          given Names()
          given Names2 = Names2(binding2)
          idx = 0

          save(expand(in, Left(end))(it -> term), ls.isEmpty && r.isEmpty) match
            case Some(_) if r.nonEmpty => throw AmbiguousParsingException
            case Some((_, in)) => r = Some((it, given_Map_String_| -> given_Names, in))
            case _ =>

        r match
          case Some(((encoding, sum), (given Map[String, λ | AST], free), in)) =>
            Success(`⟦⟧`(encoding, sum.replace.flatten) -> free, in)
          case _ => throw UndefinedParsingException
    }.named(s"""⟦${if defs.size == 1 then defs.head._1.code.toString else ""}⟧""")


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
               (using pointers: List[Symbol] = Nil): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅ => ∅

        case +(it*) =>
          `+`(it.map(_.replace)*)

        case ||(it*) =>
          ||(it.map(_.replace)*)

        case `.`(end, _it*) =>
          val it = _it.map {
            case it @ π(λ(ch: Symbol), λ(par: Symbol), true, _) =>
              it.copy(channel = replaced(ch), name = replaced(par))
            case it @ π(λ(ch: Symbol), λ(arg: Symbol), false, _) =>
              it.copy(channel = replaced(ch), name = replaced(arg))
            case it @ π(λ(ch: Symbol), _, false, _) =>
              it.copy(channel = replaced(ch))
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

        case !(Some(it @ π(λ(ch: Symbol), λ(par: Symbol), true, _)), sum) =>
          `!`(Some(it.copy(channel = replaced(ch), name = replaced(par))), sum.replace)

        case !(Some(it @ π(λ(ch: Symbol), λ(arg: Symbol), false, _)), sum) =>
          `!`(Some(it.copy(channel = replaced(ch), name = replaced(arg))), sum.replace)

        case !(Some(it @ π(λ(ch: Symbol), _, false, _)), sum) =>
          `!`(Some(it.copy(channel = replaced(ch))), sum.replace)

        case it @ !(_, sum) =>
          it.copy(sum = sum.replace)

        case it @ `⟦⟧`(_, sum, _assign) if pointers.isEmpty =>
          val assign = _assign.map(_.map(_ -> replaced(_).asSymbol))
          it.copy(sum = sum.replace, assign = assign)

        case it @ `⟦⟧`(Encoding(_, _, _, _, variables), _, assign0) =>
          val n = assign0.map(_.size).getOrElse(0)
          val assign1 = variables.drop(n) zip pointers.drop(n)
          val assign = assign0.map(_ ++ assign1).getOrElse(assign1)
          it.copy(assign = Some(assign))

        case `{}`(id, pointers) if substitution.contains(id) =>
          val pointers2 = pointers.map(replaced(_).asSymbol)
          substitution(id).asInstanceOf[&].replace(using Map.empty)(using pointers2)

        case `{}`(id, pointers0) =>
          `{}`(id, pointers0 ++ pointers)

        case it @ `(*)`(_, List("π", "this")) =>
          it

        case `(*)`(id, Nil) if substitution.contains(id) =>
          substitution(id).asInstanceOf[&]

        case `(*)`(id, qual @ List("this"), params*) =>
          val params2 = params
            .map {
              case λ(it: Symbol) => replaced(it)
              case it => it
            }

          `(*)`(id, qual, params2*)

        case `(*)`(id, qual, params*) =>
          val params2 = params
            .map {
              case λ(it: Symbol) => replaced(it)
              case it => it
            }

          `(*)`(id, qual, (params2 ++ pointers.map(λ(_)))*)