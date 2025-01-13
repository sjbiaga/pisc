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
package masc
package parser

import java.util.regex.Pattern
import scala.util.matching.Regex
import Regex.Match

import scala.collection.mutable.{ LinkedHashMap => Map, LinkedHashSet => Set }

import scala.meta.Term

import _root_.masc.parser.Expression
import Expression.Code
import _root_.masc.parser.{ Encoding, & }
import _root_.masc.parser.Ambient.{ AST => _, _ }
import _root_.masc.parser.Calculus._
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

      def expand(in: Input, shadows: List[Option[String]], key: CacheKey)
                (op: String, end: Either[String, String])
                (success: Input => (ParseResult[Fresh], Seq[Term]))
                (using binding2: Names2)
                (using substitution: Map[String, String | AST])
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
                Failure("parallel expected", in) -> Nil

              else
                val in2 = in.drop(start + n - offset)
                success(in2)

            else if op.charAt(0).isLower || op == "_"
            then

              parseAll(name, result) match

                case Success((it, free2), _) =>
                  shadows(idx) match
                    case shadow @ Some(_) =>
                      Names2Occurrence(it, shadow)
                    case _ =>
                      binding2.find { case (`it`, Shadow(_)) => true case _ => false } match
                        case Some((_, Shadow(it))) => substitution(op) = it
                        case _ => substitution(op) = it
                      free ++= free2 -- binding2.map(_._1)
                  idx += 1

                  val in2 = in.drop(start + n - offset)
                  success(in2)

                case _ =>
                  Failure("name expected", in) -> Nil

            else

              _cache.get(key) match

                case Some((par: ∥, cp, free2, given Names2, in2)) =>
                  binding2 ++= binders

                  substitution(op) = par
                  free ++= free2 -- binding2.map(_._1)

                  paste(cp)

                  success(in2)

                case _ =>

                  given Names2 = Names2(binding2)
                  parseAll(parallel, result) match

                    case Success((par, free2), _) =>
                      binding2 ++= binders

                      val par2 = par.flatten.update(using Names2(binding2))

                      substitution(op) = par2
                      free ++= free2 -- binding2.map(_._1)

                      val in2 = in.drop(start + n - offset)

                      _cache(key) = (par2, copy, free2, given_Names2, in2)

                      success(in2)

                    case _ =>
                      Failure("parallel expected", in) -> Nil

          case _ =>
            val found = if start == source.length
                        then "end of source"
                        else "'"+source.charAt(start)+"'"
            Failure(end.map("operator '" + _ + "'").orElse(end.swap).right.get+" expected but "+found+" found", in.drop(start - offset)) -> Nil


      def expand(in: Input, _ts: Seq[Term], end: Either[String, String])
                (using Names2)
                (using Map[String, String | AST])
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

        var r: Option[(Fresh, (Map[String, String | AST], (Names, Names2)), Input)] = None

        var ls = defs
        while ls.nonEmpty
        do
          val (_macro, Definition(code, Some(term), _, _, _)) = ls.head : @unchecked
          ls = ls.tail

          given Map[String, String | AST]()
          given Names()
          given Names2 = Names2(binding2)
          idx = 0

          save(expand(in, Nil, Left(end))(_macro(code, id, term) -> term), ls.isEmpty && r.isEmpty) match
            case Some(_) if r.nonEmpty => throw AmbiguousParsingException
            case Some((it @ (_, (arity, _)), in)) if arity == given_Map_String_|.size =>
              r = Some((it, given_Map_String_| -> (given_Names -> given_Names2), in))
            case _ =>

        r match

          case Some(((definition, _), (given Map[String, String | AST], (free, given Names2)), in)) =>
            binding2 ++= binders

            Success(definition() -> free, in)

          case _ => throw UndefinedParsingException

    }.named(s"""⟦${if defs.size == 1 then defs.head._2.code.toString else ""}⟧""")


object Expansion:

  private val open_r = """⟦\d*""".r
  private val closed_r = """\d*⟧""".r

  // exceptions

  import _root_.masc.parser.Expression.ParsingException

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

  def replaced(name: String)
              (using substitution: Map[String, String | AST]): String =
    substitution.get(name) match
      case Some(it: String) => it
      case _ => name

  def updated(name: String)
             (using binding2: Names2): String =
    binding2.find { case (`name`, Shadow(_)) => true case _ => false } match
      case Some((_, Shadow(it))) => it
      case _ => name

  private def recoded(using code: Option[Code])
                     (using substitution: Map[String, String | AST] = null)
                     (using updating: Names2 = null): Option[Code] =
    code.map { (_, orig) =>
      Expression(orig)._1 match
        case term @ Term.ForYield(enums, _) =>
          (Left(enums), term)
        case term =>
          (Right(term), term)
    }


  extension[T <: AST](ast: T)

    def replace(using substitution: Map[String, String | AST]): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅ => ∅

        case ∥(it*) =>
          ∥(it.map(_.replace)*)

        case `.`(end, _it*) =>
          val it = _it.map {
            case it @ τ(given Option[Code]) =>
              it.copy(code = recoded)
            case `,.`(_path*) =>
              val path = _path.map {
                case Λ(name) => Λ(replaced(name))
                case ζ(op, amb) => ζ(op, replaced(amb))
                case it => it
              }
              `,.`(path*)
            case it @ `()`(_, given Option[Code]) =>
              it.copy(code = recoded)
            case it => it
          }
          `.`(end.replace, it*)

        case <>(given Option[Code], _path*) =>
          val path = _path.map {
            case Λ(name) => Λ(replaced(name))
            case ζ(op, amb) => ζ(op, replaced(amb))
            case it => it
          }
          <>(recoded, path*)

        case it @ !(_, par) =>
          it.copy(par = par.replace)

        case `[]`(amb, par) =>
          `[]`(replaced(amb), par.replace)

        case `go.`(amb, par) =>
          `go.`(replaced(amb), par.replace)

        case it @ `⟦⟧`(_, _, par, _assign) =>
          val assign = _assign.map(_.map(_ -> replaced(_)))
          it.copy(par = par.replace, assign = assign)

        case `{}`(id, pointers, false) =>
          given List[String] = pointers.map(replaced(_))
          if given_List_String.nonEmpty
          then
            substitution(id).asInstanceOf[&].flatten.concatenate
          else
            substitution(id).asInstanceOf[&]

        case `{}`(id, pointers, true, params*) =>
          val pointers2 = pointers.map(replaced(_))
          val params2 = params.map(replaced(_))

          `{}`(id, pointers2, true, params2*)

        case _: `{}` => ???

        case `(*)`(id, qual, params*) =>
          val params2 = params.map(replaced(_))

          `(*)`(id, qual, params2*)


    private def concatenate(using pointers: List[String]): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅ => ∅

        case ∥(it*) =>
          ∥(it.map(_.concatenate)*)

        case `.`(end, it*) =>
          `.`(end.concatenate, it*)

        case it @ !(_, par) =>
          it.copy(par = par.concatenate)

        case it @ `[]`(_, par) =>
          it.copy(par = par.concatenate)

        case it @ `go.`(_, par) =>
          it.copy(par = par.concatenate)

        case it @ `⟦⟧`(_, variables, _, _) =>
          val n = it.assign.map(_.size).getOrElse(0)
          val assign = variables.drop(n) zip pointers
          val assign2 = it.assign.getOrElse(Set.empty) ++ assign
          it.copy(assign = if assign2.nonEmpty then Some(assign2) else None)

        case it @ `{}`(id, _, agent, params*) =>
          val pointers2 = it.pointers ++ pointers
          `{}`(id, pointers2, agent, params*)

        case it => it


    def update(using binding2: Names2): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅ => ∅

        case ∥(it*) =>
          ∥(it.map(_.update)*)

        case `.`(end, _it*) =>
          given Names2 = Names2(binding2)
          val it = _it.map {
            case it @ ν(names*) =>
              given_Names2 --= names
              it
            case it @ τ(given Option[Code]) =>
              it.copy(code = recoded)
            case `,.`(_path*) =>
              val path = _path.map {
                case Λ(name) => Λ(updated(name))
                case ζ(op, amb) => ζ(op, updated(amb))
                case it => it
              }
              `,.`(path*)
            case it @ `()`(name, given Option[Code]) =>
              given_Names2 -= name
              it.copy(code = recoded)
          }
          `.`(end.update, it*)

        case <>(given Option[Code], _path*) =>
          val path = _path.map {
            case Λ(name) => Λ(updated(name))
            case ζ(op, amb) => ζ(op, updated(amb))
            case it => it
          }
          <>(recoded, path*)

        case it @ !(Some(name), par) =>
          given Names2 = Names2(binding2)
          given_Names2 -= name
          it.copy(par = par.update)

        case it @ !(_, par) =>
          it.copy(par = par.update)

        case `[]`(amb, par) =>
          `[]`(updated(amb), par.update)

        case `go.`(amb, par) =>
          `go.`(updated(amb), par.update)

        case it @ `⟦⟧`(_, _, par, assign) =>
          val assign2 = assign.map(_.map(_ -> updated(_)))
          it.copy(par = par.update, assign = assign2)

        case `{}`(id, pointers, agent, params*) =>
          val pointers2 = pointers.map(updated(_))
          val params2 = params.map(updated(_))

          `{}`(id, pointers2, agent, params2*)

        case `(*)`(id, qual, params*) =>
          val params2 = params.map(updated(_))

          `(*)`(id, qual, params2*)
