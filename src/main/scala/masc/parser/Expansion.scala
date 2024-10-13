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
package masc
package parser

import java.util.regex.Pattern
import scala.util.matching.Regex

import scala.collection.mutable.{ HashMap => Map, LinkedHashSet => Set }

import scala.meta.Term

import _root_.masc.parser.{ Calculus, `&`, `name | process` }
import _root_.masc.parser.Ambient.{ AST => _, _ }
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

  def instance(defn: Define, end: String)
              (using binding2: Names2): Parser[(`⟦⟧`, Names)] =
    val (encoding @ Encoding(code, Some(term), params, _, _), par) = defn : @unchecked
    var idx = 0
    new Parser[(`⟦⟧`, Names)] {
      override def apply(in: Input): ParseResult[(`⟦⟧`, Names)] =
        def expand(in: Input, end: Either[String, String])
                  (using substitution: Map[String, `name | process`])
                  (using free: Names): Term => ParseResult[(`⟦⟧`, Names)] =

          case Term.Name(rhs) =>
            var source = in.source
            var offset = in.offset
            var start = handleWhiteSpace(source, offset)

            var s = SubSequence(source, start)

            if ("""⟦\d*""".r findPrefixMatchOf s).nonEmpty
            then
              parse(instantiation, in) match

                case Success((rhs2, free2), in) =>
                  substitution(rhs) = rhs2
                  free ++= free2 -- binding2.map(_._1)

                  source = in.source
                  offset = in.offset
                  start = handleWhiteSpace(source, offset)

                  val r = if end.isRight
                          then (null, null)
                          else `⟦⟧`(encoding, par.replace.flatten) -> free
                  Success(r, in.drop(start + end.map(_.length).getOrElse(0) - offset))

                case failure: NoSuccess =>
                  scala.sys.error(failure.msg)

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
                  if matches.nonEmpty
                  then
                    s = SubSequence(source, start, matches(i).end - n)
                    while i < matches.size && (open findAllMatchIn s).size > (closed findAllMatchIn s).size
                    do
                      i += 1
                      s = SubSequence(source, start, matches(i).end - n)
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

                  if result.isBlank
                  || result.strip == "_"
                  then
                    n += result.length
                  else if ("""\d*⟧""".r findPrefixMatchOf s).nonEmpty
                  then
                    {}
                  else
                    n += end.map(_.length).getOrElse(0)
                    if rhs.charAt(0).isLower
                    then
                      parseAll(name, result) match
                        case Success((name2, free2), _) =>
                          encoding.binding(idx) match
                            case Some(binding) =>
                              Names2(name2, Some(binding))
                            case _ =>
                              binding2.find { case (`name2`, (Right(Some(_)), _)) => true case _ => false } match
                                case Some((_, (Right(Some(binding)), _))) => substitution(rhs) = binding
                                case _ => substitution(rhs) = name2
                              free ++= free2 -- binding2.map(_._1)
                          idx += 1

                        case failure: NoSuccess =>
                          scala.sys.error(failure.msg)

                    else
                      parseAll(parallel, result) match
                        case Success((par2, free2), _) =>
                          substitution(rhs) = par2
                          free ++= free2 -- binding2.map(_._1)

                        case failure: NoSuccess =>
                          scala.sys.error(failure.msg)

                  val r = if end.isRight
                          then (null, null)
                          else `⟦⟧`(encoding, par.replace.flatten) -> free
                  Success(r, in.drop(start + n - offset))

                case _ =>
                  val found = if start == source.length
                              then "end of source"
                              else "'"+source.charAt(start)+"'"
                  Failure(end.orElse(end.swap).right.get+" expected but "+found+" found", in.drop(start - offset))

          case Term.ApplyInfix(Term.Name(lhs), Term.Name(op), _, List(rhs)) =>
            var source = in.source
            var offset = in.offset
            var start = handleWhiteSpace(source, offset)

            var s = SubSequence(source, start)

            if ("""⟦\d*""".r findPrefixMatchOf s).nonEmpty
            then
              parse(instantiation, in) match

                case Success((lhs2, free2), in) =>
                  substitution(lhs) = lhs2
                  free ++= free2 -- binding2.map(_._1)

                  source = in.source
                  offset = in.offset
                  start = handleWhiteSpace(source, offset)

                  expand(in.drop(start + op.length - offset), end)(rhs)

                case failure: NoSuccess =>
                  scala.sys.error(failure.msg)

            else {
              val n = op.length
              val open = """⟦\d*""".r
              val closed = """\d*⟧""".r
              val matches = (Pattern.quote(op).r findAllMatchIn s).toList
              var i = 0
              if matches.nonEmpty
              then
                s = SubSequence(source, start, matches(i).end - n)
                while i < matches.size && (open findAllMatchIn s).size > (closed findAllMatchIn s).size
                do
                  i += 1
                  s = SubSequence(source, start, matches(i).end - n)
              if i < matches.size then Some(matches(i)) else None
            } match

              case Some(matched) =>
                val result = source.subSequence(start, start + matched.end - op.length).toString

                if result.isBlank
                || result.strip == "_"
                then
                  {}
                else if lhs.charAt(0).isLower
                then
                  parseAll(name, result) match
                    case Success((name2, free2), _) =>
                      encoding.binding(idx) match
                        case Some(binding) =>
                          Names2(name2, Some(binding))
                        case _ =>
                          binding2.find { case (`name2`, (Right(Some(_)), _)) => true case _ => false } match
                            case Some((_, (Right(Some(binding)), _))) => substitution(lhs) = binding
                            case _ => substitution(lhs) = name2
                          free ++= free2 -- binding2.map(_._1)
                      idx += 1

                    case failure: NoSuccess =>
                      scala.sys.error(failure.msg)

                else
                  parseAll(parallel, result) match
                    case Success((par2, free2), _) =>
                      substitution(lhs) = par2
                      free ++= free2 -- binding2.map(_._1)

                    case failure: NoSuccess =>
                      scala.sys.error(failure.msg)

                expand(in.drop(start + matched.end - offset), end)(rhs)

              case _ =>
                val found = if start == source.length
                            then "end of source"
                            else "'"+source.charAt(start)+"'"
                Failure("operator '"+op+"' expected but "+found+" found", in.drop(start - offset))

          case it @ Term.ApplyInfix(lhs: Term.ApplyInfix, Term.Name(op), _, List(rhs)) =>
            expand(in, Right(op))(lhs) match

              case Success(_, _in) =>
                var in = _in
                var source = in.source
                var offset = in.offset
                var start = handleWhiteSpace(source, offset)

                var s = SubSequence(source, start)

                if s.charAt(0) == '_' && (s.length == 1 || s.charAt(1).isWhitespace)
                then
                  in = in.drop(start + 1 - offset)
                  source = in.source
                  offset = in.offset
                  start = handleWhiteSpace(source, offset)

                  s = SubSequence(source, start)

                (Pattern.quote(end.orElse(end.swap).right.get).r findPrefixMatchOf s) match

                  case Some(matched) =>
                    val r = if end.isRight
                            then (null, null)
                            else `⟦⟧`(encoding, par.replace.flatten) -> free
                    Success(r, in.drop(start + matched.end - offset))

                  case _ =>
                    expand(in, end)(rhs)

              case it => it

          case it => throw InstantiationParsingException(it)

        given Map[String, `name | process`]()
        given Names()

        expand(in, Left(end))(term)
    }.named(s"⟦$code $term $code⟧")


object Expansion:

  // exceptions

  import _root_.masc.parser.Expression.ParsingException

  case class InstantiationParsingException(it: Term)
      extends ParsingException(s"A (macro-)definition template strictly parses Term.ApplyInfix and not $it terms")


  // functions

  private def replaced(name: String)
                      (using substitution: Map[String, `name | process`]): String =
    substitution.get(name).getOrElse(name).asInstanceOf[String]


  extension[T <: AST](ast: T)

    def replace(using substitution: Map[String, `name | process`])
               (using pointers: List[String] = Nil): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case `∅` => ∅

        case `|`(it*) =>
          `|`(it.map(_.replace)*)

        case `.`(end, _it*) =>
          val it = _it.map {
            case `()`(term, name) =>
              `()`(term, replaced(name))
            case `,.`(_path*) =>
              val path = _path.map {
                case Λ(name) => Λ(replaced(name))
                case ζ(op, amb) => ζ(op, replaced(amb))
                case it => it
              }
              `,.`(path*)
            case it => it
          }
          `.`(end.replace, it*)

        case `<>`(term, _path*) =>
          val path = _path.map {
            case Λ(name) => Λ(replaced(name))
            case ζ(op, amb) => ζ(op, replaced(amb))
            case it => it
          }
          `<>`(term, path*)

        case `!`(Some(name), par) =>
          `!`(Some(replaced(name)), par.replace)

        case it @ `!`(_, par) =>
          it.copy(par = par.replace)

        case `[]`(amb, par) =>
          `[]`(replaced(amb), par.replace)

        case `go.`(amb, par) =>
          `go.`(replaced(amb), par.replace)

        case it @ `⟦⟧`(_, par, Some(_assign)) if pointers.isEmpty =>
          val assign = _assign.map(_ -> replaced(_))
          it.copy(par = par.replace, assign = Some(assign))

        case it @ `⟦⟧`(_, par, _) if pointers.isEmpty =>
          it.copy(par = par.replace)

        case it @ `⟦⟧`(Encoding(_, _, _, _, bound), _, Some(assign0)) if assign0.size < pointers.size =>
          val assign1 = bound.drop(assign0.size) zip pointers.drop(assign0.size)
          val assign = assign0 ++ assign1
          it.copy(assign = Some(assign))

        case it @ `⟦⟧`(_, _, Some(_)) =>
          it

        case it @ `⟦⟧`(Encoding(_, _, _, _, bound), _, _) =>
          val assign = bound zip pointers
          it.copy(assign = Some(assign))

        case `{}`(id, pointers) if substitution.contains(id) =>
          val pointers2 = pointers.map(replaced(_))
          substitution(id).asInstanceOf[`&`].replace(using Map.empty)(using pointers2)

        case _: `{}` => ???

        case it @ `(*)`(_, List("π", "this")) =>
          it

        case `(*)`(id, Nil) if substitution.contains(id) =>
          substitution(id).asInstanceOf[`&`]

        case `(*)`(id, qual @ List("this"), params*) =>
          val params2 = params.map(replaced(_))

          `(*)`(id, qual, params2*)

        case `(*)`(id, qual, params*) =>
          val params2 = params.map(replaced(_))

          `(*)`(id, qual, (params2 ++ pointers)*)
