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

import scala.collection.mutable.{
  HashMap => Map,
  ListBuffer => MutableList,
  LinkedHashSet => Set
}

import scala.meta.Term

import _root_.pisc.parser.{ Calculus, `&`, `name | process` }
import _root_.pisc.parser.PolyadicPi._
import Calculus._
import Extension._


class Extension extends Calculus:

  /**
    * A parser that matches a regex string and returns the Match
    * [[https://stackoverflow.com/questions/1815716/accessing-scala-parser-regular-expression-match-data]]
    */
  override def regexMatch(r: Regex): Parser[Regex.Match] = new Parser[Regex.Match] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      (r findPrefixMatchOf SubSequence(source, start)) match {
        case Some(matched) =>
          Success(matched,
                  in.drop(start + matched.end - offset))
        case None =>
          Failure("string matching regex `"+r+"' expected but `"+in.first+"' found", in.drop(start - offset))
      }
    }
  }

  override def expand(defn: Define, end: String): Parser[(`[|]`, Names)] =
    val (encoding @ Encoding(code, term, _, _), sum) = defn
    new Parser[(`[|]`, Names)] {
      override def apply(in: Input): ParseResult[(`[|]`, Names)] =
        def expand(in: Input, end: Either[String, String])
                  (using bind: Map[String, `name | process`])
                  (using free: Names): Term => ParseResult[(`[|]`, Names)] = {

          case Term.Name(rhs) =>
            var source = in.source
            var offset = in.offset
            var start = handleWhiteSpace(source, offset)

            val s = SubSequence(source, start)

            if ("""\[(\d*)\|""".r findPrefixMatchOf s).nonEmpty
            then
              parse(expansion, in) match

                case Success((rhs2, free2), in) =>
                  bind(rhs) = Calculus.`+`(Calculus.`|`(`.`(rhs2)))
                  free ++= free2

                  source = in.source
                  offset = in.offset
                  start = handleWhiteSpace(source, offset)

                  val r = if end.isRight
                          then (null, null)
                          else
                            given MutableList[(String, λ)]()
                            `[|]`(encoding, sum.bind.rename.asInstanceOf[`+`]) -> free
                  Success(r, in.drop(start + end.map(_.length).getOrElse(0) - offset))

                case failure: NoSuccess =>
                  scala.sys.error(failure.msg)

            else
              ( if s.isEmpty
                || s.charAt(0) == '_' && (s.length == 1 || s.charAt(1).isWhitespace)
                || ("""\|(\d*)\]""".r findPrefixMatchOf s).nonEmpty
                then Some(null)
                else
                  Pattern.quote(end.orElse(end.swap).right.get).r findFirstMatchIn s
              ) match

                case Some(matched) =>
                  var n = ( if matched eq null
                            then
                              if s.charAt(0) == '_' && (s.length == 1 || s.charAt(1).isWhitespace)
                              then 1
                              else 0
                            else matched.end - end.orElse(end.swap).map(_.length).right.get
                          )
                  val result = source.subSequence(start, start + n).toString

                  if result.isBlank
                  || result.strip == "_"
                  then
                    n += result.length
                  else if ("""\|(\d*)\]""".r findPrefixMatchOf s).nonEmpty
                  then
                    {}
                  else
                    n += end.map(_.length).getOrElse(0)
                    if rhs.charAt(0).isLower
                    then
                      parseAll(name, result) match
                        case Success((name2, free2), _) =>
                          bind(rhs) = name2
                          free ++= free2
                        case failure: NoSuccess =>
                          scala.sys.error(failure.msg)
                    else
                      parseAll(choice, result) match
                        case Success((sum2, free2), _) =>
                          bind(rhs) = sum2
                          free ++= free2
                        case failure: NoSuccess =>
                          scala.sys.error(failure.msg)

                  val r = if end.isRight
                          then (null, null)
                          else
                            given MutableList[(String, λ)]()
                            `[|]`(encoding, sum.bind.rename.asInstanceOf[`+`]) -> free
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

            val s = SubSequence(source, start)

            if ("""\[(\d*)\|""".r findPrefixMatchOf s).nonEmpty
            then
              parse(expansion, in) match

                case Success((lhs2, free2), in) =>
                  bind(lhs) = Calculus.`+`(Calculus.`|`(`.`(lhs2)))
                  free ++= free2

                  source = in.source
                  offset = in.offset
                  start = handleWhiteSpace(source, offset)

                  expand(in.drop(start + op.length - offset), end)(rhs)

                case failure: NoSuccess =>
                  scala.sys.error(failure.msg)

            else (Pattern.quote(op).r findFirstMatchIn s) match

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
                      bind(lhs) = name2
                      free ++= free2
                    case failure: NoSuccess =>
                      scala.sys.error(failure.msg)
                else
                  parseAll(choice, result) match
                    case Success((sum2, free2), _) =>
                      bind(lhs) = sum2
                      free ++= free2
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
                            else
                              given MutableList[(String, λ)]()
                              `[|]`(encoding, sum.bind.rename.asInstanceOf[`+`]) -> free
                    Success(r, in.drop(start + matched.end - offset))

                  case _ =>
                    expand(in, end)(rhs)

              case it => it

          case it => throw ExpansionParsingException(it)
        }

        given Map[String, `name | process`]()
        given Names()

        expand(in, Left(end))(term)
    }.named(s"[$code| $term |$code]")


object Extension:

  // exceptions

  import _root_.pisc.parser.Expression.ParsingException

  case class ExpansionParsingException(it: Term)
      extends ParsingException(s"A (macro-)definition template strictly parses Term.ApplyInfix and not $it terms")


  // functions

  extension(ast: AST)

    def bind(using b: Map[String, `name | process`])
            (using free: Names): AST =

      inline given Conversion[`name | process`, λ] = _.asInstanceOf[λ]

      ast match

        case `∅` => ∅

        case `+`(it*) =>
          `+`(it.map(_.bind.asInstanceOf[`|`])*)

        case `|`(it*) =>
          `|`(it.map(_.bind.asInstanceOf[`.`])*)

        case `.`(end, _it*) =>
          val it = _it.map {
            case π(λ(Symbol(ch)), true, code, _names*) =>
              val names: Seq[λ] = _names.map { case it @ λ(Symbol(par)) =>
                val par2 = if b.contains(par) then b(par) else it
                if b.contains(par) then free -= par2.asSymbol
                par2
              }
              val ch2 = if b.contains(ch) then b(ch) else λ(Symbol(ch))
              π(ch2, true, code, names*)
            case π(λ(Symbol(ch)), false, code, _names*) =>
              val names: Seq[λ] = _names.map { case it @ λ(Symbol(arg)) =>
                if b.contains(arg) then b(arg) else it
              }
              val ch2 = if b.contains(ch) then b(ch) else λ(Symbol(ch))
              π(ch2, false, code, names*)
            case it => it
          }
          `.`(end.bind.asInstanceOf[`&`], it*)

        case `?:`(((λ(Symbol(lhs)), λ(Symbol(rhs))), m), t, f) =>
          val lhs2 = if b.contains(lhs) then b(lhs) else λ(Symbol(lhs))
          val rhs2 = if b.contains(rhs) then b(rhs) else λ(Symbol(rhs))
          `?:`(((lhs2, rhs2), m), t.bind.asInstanceOf[`+`], f.bind.asInstanceOf[`+`])

        case `?:`(((λ(Symbol(lhs)), rhs), m), t, f) =>
          val lhs2 = if b.contains(lhs) then b(lhs) else λ(Symbol(lhs))
          `?:`(((lhs2, rhs), m), t.bind.asInstanceOf[`+`], f.bind.asInstanceOf[`+`])

        case `?:`(((lhs, λ(Symbol(rhs))), m), t, f) =>
          val rhs2 = if b.contains(rhs) then b(rhs) else λ(Symbol(rhs))
          `?:`(((lhs, rhs2), m), t.bind.asInstanceOf[`+`], f.bind.asInstanceOf[`+`])

        case `?:`(cond, t, f) =>
          `?:`(cond, t.bind.asInstanceOf[`+`], f.bind.asInstanceOf[`+`])

        case `!`(Some(π(λ(Symbol(ch)), true, code, _names*)), sum) =>
          val names: Seq[λ] = _names.map { case it @ λ(Symbol(par)) =>
            val par2 = if b.contains(par) then b(par) else it
            if b.contains(par) then free -= par2.asSymbol
            par2
          }
          val ch2 = if b.contains(ch) then b(ch) else λ(Symbol(ch))
          `!`(Some(π(ch2, true, code, names*)), sum.bind.asInstanceOf[`+`])

        case `!`(Some(π(λ(Symbol(ch)), false, code, _names*)), sum) =>
          val names: Seq[λ] = _names.map { case it @ λ(Symbol(arg)) =>
            if b.contains(arg) then b(arg) else it
          }
          val ch2 = if b.contains(ch) then b(ch) else λ(Symbol(ch))
          `!`(Some(π(ch2, false, code, names*)), sum.bind.asInstanceOf[`+`])

        case `!`(guard, sum) =>
          `!`(guard, sum.bind.asInstanceOf[`+`])

        case `[|]`(encoding, sum, assign) =>
          `[|]`(encoding, sum.bind.asInstanceOf[`+`], assign)

        case `{}`(id, pointers) if b.contains(id) =>
          b(id).asInstanceOf[`+`] match
            case `+`(`|`(`.`(it @ `[|]`(Encoding(_, _, _, bound), _, _)))) =>
              `+`(`|`(`.`(it.copy(assign = Some(bound.map(_.name) zip pointers.map(_.name))))))
            case `+`(`|`(`.`(`(*)`(identifier, qual, params*)))) =>
              `+`(`|`(`.`(`(*)`(identifier, qual, (params ++ pointers.map(λ(_)))*))))
            case it => it

        case _: `{}` => ???

        case `(*)`(id, Nil) if b.contains(id) =>
          b(id).asInstanceOf[`+`]

        case `(*)`(id, qual, params*) =>
          val args: Seq[λ] = params
            .map {
              case λ(Symbol(it)) if b.contains(it) => b(it)
              case it => it
            }

          `(*)`(id, qual, args*)

  extension[T <: AST](ast: T)

    def rename(using r: MutableList[(String, λ)]): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case `∅` => ∅

        case `+`(it*) =>
          `+`(it.map(_.rename)*)

        case `|`(it*) =>
          `|`(it.map(_.rename)*)

        case `.`(end, _it*) =>
          val it = _it.map {
            case π(λ(Symbol(ch)), true, code, _names*) =>
              val names = _names.map { case it @ λ(Symbol(par)) =>
                val υidυ = par.replaceAll("_υ.*υ", "") + id
                r.prepend(par -> λ(Symbol(υidυ)))
                r.find(_._1 == par).get._2
              }
              val ch2 = r.find(_._1 == ch).map(_._2).getOrElse(λ(Symbol(ch)))
              π(ch2, true, code, names*)
            case π(λ(Symbol(ch)), false, code, _names*) =>
              val names = _names.map { case it @ λ(Symbol(arg)) =>
                r.find(_._1 == arg).map(_._2).getOrElse(it)
              }
              val ch2 = r.find(_._1 == ch).map(_._2).getOrElse(λ(Symbol(ch)))
              π(ch2, false, code, names*)
            case ν(names*) =>
              names
                .reverse
                .foreach { (_, it) =>
                  val υidυ = it.replaceAll("_υ.*υ", "") + id
                  r.prepend(it -> λ(Symbol(υidυ)))
                }
              ν(names.map { (cap, it) => cap -> r.find(_._1 == it).get._2.asSymbol.name }*)
            case it => it
          }
          val seq = `.`(end.rename, it*)
          it.reverse.foreach {
            case π(_, true, _, names*) =>
              r.remove(0, names.size)
            case ν(names*) =>
              r.remove(0, names.size)
            case _ =>
          }
          seq

        case `?:`(((λ(Symbol(lhs)), λ(Symbol(rhs))), m), t, f) =>
          val lhs2 = r.find(_._1 == lhs).map(_._2).getOrElse(λ(Symbol(lhs)))
          val rhs2 = r.find(_._1 == rhs).map(_._2).getOrElse(λ(Symbol(rhs)))
          `?:`(((lhs2, rhs2), m), t.rename, f.rename)

        case `?:`(((λ(Symbol(lhs)), rhs), m), t, f) =>
          val lhs2 = r.find(_._1 == lhs).map(_._2).getOrElse(λ(Symbol(lhs)))
          `?:`(((lhs2, rhs), m), t.rename, f.rename)

        case `?:`(((lhs, λ(Symbol(rhs))), m), t, f) =>
          val rhs2 = r.find(_._1 == rhs).map(_._2).getOrElse(λ(Symbol(rhs)))
          `?:`(((lhs, rhs2), m), t.rename, f.rename)

        case `?:`(cond, t, f) =>
          `?:`(cond, t.rename, f.rename)

        case `!`(Some(it @ π(λ(Symbol(ch)), true, code, _names*)), sum) =>
          val names = _names.map { case it @ λ(Symbol(par)) =>
            val υidυ = par.replaceAll("_υ.*υ", "") + id
            r.prepend(par -> λ(Symbol(υidυ)))
            r.find(_._1 == par).get._2
          }
          val ch2 = r.find(_._1 == ch).map(_._2).getOrElse(λ(Symbol(ch)))
          val rep = `!`(Some(π(ch2, true, code, names*)), sum.rename)
          r.remove(0, names.size)
          rep

        case `!`(Some(it @ π(λ(Symbol(ch)), false, code, _names*)), sum) =>
          val names = _names.map { case it @ λ(Symbol(arg)) =>
            r.find(_._1 == arg).map(_._2).getOrElse(it)
          }
          val ch2 = r.find(_._1 == ch).map(_._2).getOrElse(λ(Symbol(ch)))
          `!`(Some(π(ch2, false, code, names*)), sum.rename)

        case `!`(guard, sum) =>
          `!`(guard, sum.rename)

        case `[|]`(encoding, sum, Some(assign)) =>
          val assign2 = assign
            .map { case (bound, pointer) =>
              val pointer2 = r.find(_._1 == pointer).map(_._2.asSymbol.name).getOrElse(pointer)
              val υidυ = bound.replaceAll("_υ.*υ", "") + id
              r.prepend(bound -> λ(Symbol(υidυ)))
              val bound2 = r.find(_._1 == bound).get._2.asSymbol.name
              bound2 -> pointer2
            }
          val encoding2 = encoding.copy(bound = assign2.map(_._1).map(Symbol(_)))
          val enc = `[|]`(encoding2, sum.rename, Some(assign2))
          r.remove(0, assign.size)
          enc

        case `[|]`(encoding, sum, _) =>
          `[|]`(encoding, sum.rename, None)

        case _: `{}` => ???

        case `(*)`(id, qual, params*) =>
          val args = params
            .map {
              case λ(Symbol(it)) => r.find(_._1 == it).map(_._2).getOrElse(λ(Symbol(it)))
              case it => it
            }

          `(*)`(id, qual, args*)

  private var _id = scala.collection.mutable.Seq('0')
  private var _ix = 0
  /**
    * @return unique identifiers of the form "_υ[0-9a-zA-Z]+υ"
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