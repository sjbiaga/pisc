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

import scala.collection.mutable.{ LinkedHashSet => Set }

import scala.meta._

import scala.util.parsing.combinator._

import StochasticPi.Names
import Expression.ExpressionParsingException


class Expression extends JavaTokenParsers:

  /** Scala comment enclosing any [[Scalameta]] term
    * or [[Enumerator]]s (used for assignment)
    * @return
    */
  def expression: Parser[(Either[List[Enumerator], Term], Names)] =
    """[/][*].*?[*][/]""".r ^^ { it =>
      val expr = it.stripPrefix("/*").stripSuffix("*/")
      try
        val (term, names) = Expression(expr.parse[Term].get)
        Right(term) -> names
      catch _ =>
        try
          val (stat, rhs) = Expression(("for {" + expr + " } yield ()").parse[Stat].get)
          stat match
            case Term.ForYield(enums, _) =>
              val lhs = enums
                .filter {
                  case Enumerator.Generator(pat, _) =>
                    pat.isInstanceOf[Lit.Symbol]
                  case Enumerator.CaseGenerator(pat, _) =>
                    pat.isInstanceOf[Lit.Symbol]
                  case Enumerator.Val(pat, _) =>
                    pat.isInstanceOf[Lit.Symbol]
                  case _ => false
                }.map {
                  case Enumerator.Generator(pat, _) =>
                    pat.asInstanceOf[Lit.Symbol]
                  case Enumerator.CaseGenerator(pat, _) =>
                    pat.asInstanceOf[Lit.Symbol]
                  case Enumerator.Val(pat, _) =>
                    pat.asInstanceOf[Lit.Symbol]
                  case _ => ???
                }
                .map(_.value)
              Left { enums.map {
                      case it @ Enumerator.Generator(pat, _)
                          if pat.isInstanceOf[Lit.Symbol] =>
                        it.copy(pat = Pat.Var(Term.Name(pat.asInstanceOf[Lit.Symbol].value.name)))
                      case it @ Enumerator.CaseGenerator(pat, _)
                          if pat.isInstanceOf[Lit.Symbol] =>
                        it.copy(pat = Pat.Var(Term.Name(pat.asInstanceOf[Lit.Symbol].value.name)))
                      case it @ Enumerator.Val(pat, _)
                          if pat.isInstanceOf[Lit.Symbol] =>
                        it.copy(pat = Pat.Var(Term.Name(pat.asInstanceOf[Lit.Symbol].value.name)))
                      case it => it
                    }
              } -> Set.from(lhs ++ rhs)
        catch t =>
          throw ExpressionParsingException(expr, t)
    }


object Expression:

  class ParsingException(msg: String, cause: Throwable = null)
      extends RuntimeException(msg, cause)

  case class ExpressionParsingException(expr: String, cause: Throwable)
      extends ParsingException(s"Expression `$expr' is not a valid Scalameta Term or Enumerator", cause)

  def apply(self: Term.Param): (Term.Param, Names) = self match
    case it @ Term.Param(_, _, _, default) =>
      if default.nonEmpty
      then
        val r = apply(default.get)
        it.copy(default = Some(r._1)) -> r._2
      else
        it -> Names()

  def apply(self: List[List[Term.Param]]): (List[List[Term.Param]], Names) =
      self.foldLeft(List[List[Term.Param]]() -> Names()) {
        case ((ls, names), ps) =>
          val rs = ps.map(apply(_))
          val r2 = rs.map(_._2).foldLeft(Names())(_ ++ _)
          (ls :+ rs.map(_._1), names ++ r2)
      }

  def apply(self: Term.ParamClause): (Term.ParamClause, Names) = self match
    case it @ Term.ParamClause(values, _) =>
      val rs = values.map(apply(_))
      val r2 = rs.map(_._2).foldLeft(Names())(_ ++ _)
      it.copy(values = rs.map(_._1)) -> r2

  def apply(self: Member.ParamClauseGroup): (Member.ParamClauseGroup, Names) = self match
    case it @ Member.ParamClauseGroup(_, ls) =>
      val rs = ls.map(apply(_))
      val r2 = rs.map(_._2).foldLeft(Names())(_ ++ _)
      it.copy(paramClauses = rs.map(_._1)) -> r2


  def apply(self: Init): (Init, Names) = self match
    case it @ Init(_, _, args) =>
      val rs = args.foldLeft(List[List[Term]]() -> Names()) {
        case ((ls, names), ts) =>
          val rs = ts.map(apply(_))
          val r2 = rs.map(_._2).foldLeft(Names())(_ ++ _)
          (ls :+ rs.map(_._1), names ++ r2)
      }
      it.copy(argss = rs._1) -> rs._2


  def apply(self: Template): (Template, Names) = self match
    case it @ Template(early, inits, _, stats) =>
      val es = early.map(apply(_))
      val e2 = es.map(_._2).foldLeft(Names())(_ ++ _)
      val is = inits.map(apply(_))
      val i2 = is.map(_._2).foldLeft(Names())(_ ++ _)
      val ss = stats.map(apply(_))
      val s2 = ss.map(_._2).foldLeft(Names())(_ ++ _)
      it.copy(early = es.map(_._1), inits = is.map(_._1), stats = ss.map(_._1)) -> (e2 ++ i2 ++ s2)


  def apply(self: Enumerator): (Enumerator, Names) = self match
    case it @ Enumerator.Generator(_, body) =>
      val b = apply(body)
      it.copy(rhs = b._1) -> b._2
    case it @ Enumerator.CaseGenerator(_, body) =>
      val b = apply(body)
      it.copy(rhs = b._1) -> b._2
    case it @ Enumerator.Val(_, body) =>
      val b = apply(body)
      it.copy(rhs = b._1) -> b._2
    case it @ Enumerator.Guard(body) =>
      val b = apply(body)
      it.copy(cond = b._1) -> b._2


  def apply(self: Stat): (Stat, Names) = self match

    case it @ Defn.Val(_, _, _, rhs) =>
      val r = apply(rhs)
      it.copy(rhs = r._1) -> r._2

    case it @ Defn.Var(_, _, _, rhs) =>
      if rhs.nonEmpty
      then
        val r = apply(rhs.get)
        it.copy(rhs = Some(r._1)) -> r._2
      else
        it -> Names()

    case it @ Defn.Def(_, _, _, _, _, body) =>
      val b = apply(body)
      it.copy(body = b._1) -> b._2

    case it @ Defn.Macro(_, _, _, ls, _, body) =>
      val rs = apply(ls)
      val b = apply(body)
      it.copy(paramss = rs._1, body = b._1) -> (rs._2 ++ b._2)

    case it @ Defn.Enum(_, _, _, _, template) =>
      val t = apply(template)
      it.copy(templ = t._1) -> t._2

    case it @ Defn.EnumCase(_, _, _, _, inits) =>
      val is = inits.map(apply(_))
      val i2 = is.map(_._2).foldLeft(Names())(_ ++ _)
      it.copy(inits = is.map(_._1)) -> i2

    case it: Defn.RepeatedEnumCase => it -> Names()

    case it @ Defn.Given(_, _, _, opt, template) =>
      val t = apply(template)
      it.copy(templ = t._1) -> t._2

    case it @ Defn.GivenAlias(_, _, _, ls, _, body) =>
      val rs = apply(ls)
      val b = apply(body)
      it.copy(sparams = rs._1, body = b._1) -> (rs._2 ++ b._2)

    case it @ Defn.ExtensionGroup(_, ls, stat) =>
      val rs = apply(ls)
      val s = apply(stat)
      it.copy(paramss = rs._1, body = s._1) -> (rs._2 ++ s._2)

    case it: Term => apply(it)

    case it => ???


  def apply(self: Case): (Case, Names) = self match
    case it @ Case(_, cond, body) =>
      val b = apply(body)
      if cond.nonEmpty
      then
        val c = apply(cond.get)
        it.copy(cond = Some(c._1), body = b._1) -> (c._2 ++ b._2)
      else
        it.copy(body = b._1) -> b._2


  def apply(self: Term.ArgClause): (Term.ArgClause, Names) =
      val rs = self.values.map(apply(_))
      val r2 = rs.map(_._2).foldLeft(Names())(_ ++ _)
      self.copy(values = rs.map(_._1)) -> r2


  def apply(self: Term): (Term, Names) = self match

    case it: Term.This => it -> Names()

    case it: Term.Super => it -> Names()

    case it: Term.Name => it -> Names()

    case it @ Term.Select(qual, _) =>
      val l = apply(qual)
      it.copy(qual = l._1) -> l._2

    case it @ Term.ApplyUnary(_, arg) =>
      val l = apply(arg)
      it.copy(arg = l._1) -> l._2

    case it @ Term.Apply(fun, ac) =>
      val l = apply(fun)
      val r = apply(ac)
      it.copy(fun = l._1, argClause = r._1) -> (l._2 ++ r._2)

    case it @ Term.ApplyType(fun, _) =>
      val l = apply(fun)
      it.copy(fun = l._1) -> l._2

    case it @ Term.ApplyInfix(lhs, _, _, ac) =>
      val l = apply(lhs)
      val r = apply(ac)
      it.copy(lhs = l._1, args = r._1) -> (l._2 ++ r._2)

    case it @ Term.Assign(lhs, rhs) =>
      val l = apply(lhs)
      val r = apply(rhs)
      it.copy(lhs = l._1, rhs = r._1) -> (l._2 ++ r._2)

    case it @ Term.Return(expr) =>
      val r = apply(expr)
      it.copy(expr = r._1) -> r._2

    case it @ Term.Throw(expr) =>
      val r = apply(expr)
      it.copy(expr = r._1) -> r._2

    case it @ Term.Ascribe(expr, _) =>
      val l = apply(expr)
      it.copy(expr = l._1) -> l._2

    case it @ Term.Annotate(expr, _) =>
      val l = apply(expr)
      it.copy(expr = l._1) -> l._2

    case it @ Term.Tuple(args) =>
      val rs = args.map(apply(_))
      val r2 = rs.map(_._2).foldLeft(Names())(_ ++ _)
      it.copy(args = rs.map(_._1)) -> r2

    case it @ Term.Block(stats) =>
      val rs = stats.map(apply(_))
      val r2 = rs.map(_._2).foldLeft(Names())(_ ++ _)
      it.copy(stats = rs.map(_._1)) -> r2

    case it @ Term.If(cond, thenp, elsep) =>
      val c = apply(cond)
      val t = apply(thenp)
      val e = apply(elsep)
      it.copy(cond = c._1, thenp = t._1, elsep = e._1) -> (c._2 ++ t._2 ++ e._2)

    case it @ Term.Match(expr, cases) =>
      val l = apply(expr)
      val rs = cases.map(apply(_))
      val r2 = rs.map(_._2).foldLeft(Names())(_ ++ _)
      it.copy(expr = l._1, cases = rs.map(_._1)) -> (l._2 ++ r2)

    case it @ Term.Try(expr, catchp, finallyp) =>
      val l = apply(expr)
      val rs = catchp.map(apply(_))
      val r2 = rs.map(_._2).foldLeft(Names())(_ ++ _)
      if finallyp.nonEmpty
      then
        val f = apply(finallyp.get)
        it.copy(expr = l._1, catchp = rs.map(_._1), finallyp = Some(f._1)) -> (l._2 ++ r2 ++ f._2)
      else
        it.copy(expr = l._1, catchp = rs.map(_._1)) -> (l._2 ++ r2)

    case it @ Term.TryWithHandler(expr, catchp, finallyp) =>
      val l = apply(expr)
      val r = apply(catchp)
      if finallyp.nonEmpty
      then
        val f = apply(finallyp.get)
        it.copy(expr = l._1, catchp = r._1, finallyp = Some(f._1)) -> (l._2 ++ r._2 ++ f._2)
      else
        it.copy(expr = l._1, catchp = r._1) -> (l._2 ++ r._2)

    case _: Term.Function => ???

    case it @ Term.AnonymousFunction(body) =>
      val r = apply(body)
      it.copy(body = r._1) -> r._2

    case it @ Term.PartialFunction(cases) =>
      val rs = cases.map(apply(_))
      val r2 = rs.map(_._2).foldLeft(Names())(_ ++ _)
      it.copy(cases = rs.map(_._1)) -> r2

    case it @ Term.While(expr, body) =>
      val e = apply(expr)
      val b = apply(body)
      it.copy(expr = e._1, body = b._1) -> (e._2 ++ b._2)

    case it @ Term.Do(expr, body) =>
      val e = apply(expr)
      val b = apply(body)
      it.copy(expr = e._1, body = b._1) -> (e._2 ++ b._2)

    case it @ Term.For(_, body) =>
      val b = apply(body)
      it.copy(body = b._1) -> b._2

    case it @ Term.ForYield(enums, body) =>
      val rs = enums.map(apply)
      val r2 = rs.map(_._2).foldLeft(Names())(_ ++ _)
      val b = apply(body)
      it.copy(enums = rs.map(_._1), body = b._1) -> (r2 ++ b._2)

    case it @ Term.New(init) =>
      val r = apply(init)
      it.copy(init = r._1) -> r._2

    case it @ Term.NewAnonymous(template) =>
        val r = apply(template)
        it.copy(templ = r._1) -> r._2

    case it @ Term.Placeholder() => it -> Names()

    case it @ Term.Eta(expr) =>
      val r = apply(expr)
      it.copy(expr = r._1) -> r._2

    case it @ Term.Repeated(expr) =>
      val r = apply(expr)
      it.copy(expr = r._1) -> r._2

    case it @ Term.Interpolate(_, _, args) =>
      val rs = args.map(apply(_))
      val r2 = rs.map(_._2).foldLeft(Names())(_ ++ _)
      it.copy(args = rs.map(_._1)) -> r2

    case it @ Term.Xml(_, args) =>
      val rs = args.map(apply(_))
      val r2 = rs.map(_._2).foldLeft(Names())(_ ++ _)
      it.copy(args = rs.map(_._1)) -> r2

    case it @ Term.ApplyUsing(fun, ac) =>
      val l = apply(fun)
      val r = apply(ac)
      it.copy(fun = l._1, argClause = r._1) -> (l._2 ++ r._2)

    case it @ Term.QuotedMacroExpr(body) =>
      val b = apply(body)
      it.copy(body = b._1) -> b._2

    case it @ Term.SplicedMacroExpr(body) =>
      val b = apply(body)
      it.copy(body = b._1) -> b._2

    case it @ Term.ContextFunction(params, body) =>
      val rs = params.map(apply(_))
      val r2 = rs.map(_._2).foldLeft(Names())(_ ++ _)
      val r = apply(body)
      it.copy(rs.map(_._1), body = r._1) -> (r2 ++ r._2)

    case it @ Term.PolyFunction(_, body) =>
      val r = apply(body)
      it.copy(body = r._1) -> r._2

    case it @ Lit.Symbol(free @ Symbol(name)) => Term.Name(name) -> Set(free)

    case it => it -> Names()
