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

package basc
package emitter
package fs2

import scala.meta.*
import dialects.Scala3

import parser.Calculus.`(*)`


object Meta extends emitter.shared.streams.Meta:

  override protected lazy val \ = "Stream"

  override protected lazy val \\ = "emit"

  override protected lazy val \\\ = "eval"


  def defn(body: Term)(using Type): `(*)` => Defn.Def =
    case `(*)`("Main") =>
      Defn.Def(Nil,
               "Main", `String*`("args"), `: \\[F, Unit]`,
               body)
    case `(*)`(identifier, _params*) =>
      val params = _params.map(_.asSymbol.name)
      Defn.Def(Nil,
               identifier, `(…)`(params*), `: \\[F, Unit]`,
               body)


  def `String*`(* : String)(using `)(`: Type) =
    Member.ParamClauseGroup(
      Type.ParamClause(Nil),
      Term.ParamClause(Term.Param(Nil, \(")("), Some(`)(`), None) ::
                       Term.Param(Nil, \("}{"), Some(Type.Apply(\\("}{"), Type.ArgClause(\\("F") :: Nil))), None) :: Nil) ::
      Term.ParamClause(Term.Param(Nil, *, Some(Type.Repeated(\\("String"))), None) :: Nil) ::
      `(using String)(using %[F], /[F], \\[F])(using }{.][, }{.stm.TSemaphore)`
    ) :: Nil

  def `(…)`(* : String*)(using `)(`: Type) =
    Member.ParamClauseGroup(
      Type.ParamClause(Nil),
      Term.ParamClause(Term.Param(Nil, \(")("), Some(`)(`), None) ::
                       Term.Param(Nil, \("}{"), Some(Type.Apply(\\("}{"), Type.ArgClause(\\("F") :: Nil))), None) :: Nil) ::
      Term.ParamClause(*.map(Term.Param(Nil, _, Some(Type.Apply(\\("()"), Type.ArgClause(\\("F") :: Nil))), None)).toList) ::
      `(using String)(using %[F], /[F], \\[F])(using }{.][, }{.stm.TSemaphore)`
    ) :: Nil

  val `(using String)(using %[F], /[F], \\[F])(using }{.][, }{.stm.TSemaphore)` =
    Term.ParamClause(Term.Param(Mod.Using() :: Nil,
                                Name.Anonymous(), Some(\\("String")),
                                None) :: Nil
                    ,Some(Mod.Using())) ::
    Term.ParamClause(List("%", "/", "\\")
                       .map { it => Term.Param(Mod.Using() :: Nil,
                                               Name.Anonymous(), Some(Type.Apply(\\(it), Type.ArgClause(\\("F") :: Nil))),
                                               None)
                       }
                    ,Some(Mod.Using())) ::
    Term.ParamClause(Term.Param(Mod.Using() :: Nil, Name.Anonymous(), Some(Type.Select("}{", \\("]["))), None) ::
                     Term.Param(Mod.Using() :: Nil, Name.Anonymous(), Some(Type.Select(Term.Select("}{", "stm"),
                                                                                       \\("TSemaphore"))), None) :: Nil
                    ,Some(Mod.Using())) ::
    Nil


  def `List( *, … ).parSequence`(* : Term*): Term =
    *.flatMap {
      case Term.Select(Term.Name(`\\`), Term.Name("unit")) => None
      case it => Some(it)
    } match
      case Nil => \(Nil)
      case it => Term.Select(Term.Apply(\("πLs"), Term.ArgClause(it.toList)), "πparSequence")


  val `: String ?=> \\[F, Unit]` =
    `: \\[F, Unit]`.map(Type.ContextFunction(Type.FuncParamClause(\\("String") :: Nil), _))

  def `\\.\\\\ { def *(*: ()[F]): String ?=> \\[F, Unit] = …; * }`(* : (String, String), `…`: Term): Term =
    Term.Apply(Term.Select(\, \\),
               Term.ArgClause(
                 Term.Block(
                   Defn.Def(Nil,
                            *._1,
                            Member.ParamClauseGroup(Type.ParamClause(Nil),
                                                    Term.ParamClause(Term.Param(Nil,
                                                                                *._2,
                                                                                Some(Type.Apply(\\("()"), Type.ArgClause(\\("F") :: Nil))),
                                                                                None) :: Nil, None) :: Nil) :: Nil,
                            `: String ?=> \\[F, Unit]`,
                            `…`
                   ) :: Term.Ascribe(\(*._1), Type.Apply(\\("Π-Function1"), Type.ArgClause(\\("F") :: Nil))) :: Nil
                 ) :: Nil
               )
    )

  def `\\.\\\\ { def *(): String ?=> \\[F, Unit] = …; * }`(* : String, `…`: Term): Term =
    Term.Apply(Term.Select(\, \\),
               Term.ArgClause(
                 Term.Block(
                   Defn.Def(Nil,
                            *,
                            Member.ParamClauseGroup(Type.ParamClause(Nil),
                                                    Term.ParamClause(Nil) :: Nil) :: Nil,
                            `: String ?=> \\[F, Unit]`,
                            `…`
                   ) :: Term.Ascribe(\(*), Type.Apply(\\("Π-Function0"), Type.ArgClause(\\("F") :: Nil))) :: Nil
                 ) :: Nil
               )
    )


  private def `given String = ^._2`(using ^ : (Enumerator.Generator, Term.Name)) =
    Enumerator.Val(Pat.Given(\\("String")), ^._2)

  private def `_ <- +`(parallelism: Int,
                       deferred: String,
                       cbarrier: String,
                       name: String,
                       remaining: String,
                       acquire: String,
                       release: String,
                       replication: Term,
                       sum: List[Enumerator])
                      (using ^ : (Enumerator.Generator, Term.Name)): List[Enumerator] =
    val definition =
      Defn.Def(
        Nil,
        name,
        Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          Term.ParamClause(Term.Param(Nil, remaining, Some(\\("Int")), None)
                        :: Term.Param(Nil, acquire, Some(Type.Apply(\\("Option"), Type.ArgClause(Type.Apply(\\("Semaphore"), Type.ArgClause(\\("F") :: Nil)) :: Nil))), None) :: Nil)
       :: Term.ParamClause(Term.Param(Mod.Using() :: Nil,
                                      Name.Anonymous(), Some(\\("String")),
                                      None) :: Nil) :: Nil
        ) :: Nil,
        `: \\[F, Unit]`,
        `for * yield ()`(`* <- Semaphore(…)`(release, 0)
                      :: ^._1
                      :: `_ <- *`(Term.Apply(Term.Select(
                                               `for * yield ()`(`_ <- *`(Term.Apply(replication,
                                                                                    Term.ArgClause(\(deferred) :: \(cbarrier) :: \(acquire) :: \(release) :: Nil))) :: `given String = ^._2` :: sum*),
                                               "concurrently"),
                                             Term.ArgClause(Term.If(Term.ApplyInfix(\(remaining), \("=="), Type.ArgClause(Nil), Term.ArgClause(Lit.Int(1) :: Nil)),
                                                                    Term.Select(\, "empty"),
                                                                    Term.Apply(Term.Apply(\(name),
                                                                                          Term.ArgClause(Term.ApplyInfix(\(remaining), \("-"), Type.ArgClause(Nil), Term.ArgClause(Lit.Int(1) :: Nil))
                                                                                                           :: Term.Apply(\("Some"), Term.ArgClause(\(release) :: Nil)) :: Nil)),
                                                                               Term.ArgClause(^._2 :: Nil, Some(Mod.Using())))) :: Nil)))*)
      )

    `* <- Stream.eval(*)`(deferred, `*[F]`("Deferred", \\("Boolean"))) ::
    `* <- Stream.eval(*)`(cbarrier, Term.Apply(`*[F]`("CyclicBarrier"), Term.ArgClause(Lit.Int(parallelism) :: Nil))) ::
    `* <- *`(name -> Term.Apply(Term.Select(\, \\), Term.ArgClause(Term.Block(definition :: \(name) :: Nil) :: Nil))) ::
    `_ <- *`(Term.Apply(\(name), Term.ArgClause(Lit.Int(parallelism) :: \("None") :: Nil))) :: Nil

  def `_ <- +`(parallelism: Int, replication: Term, sum: List[Enumerator])
              (using id: => String, ^ : (Enumerator.Generator, Term.Name)): List[Enumerator] =
    `_ <- +`(parallelism, id, id, id, id, id, id, replication, sum)

  private def `* <- +`(parameter: String,
                       parallelism: Int,
                       deferred: String,
                       cbarrier: String,
                       name: String,
                       remaining: String,
                       acquire: String,
                       release: String,
                       replication: Term,
                       sum: List[Enumerator])
                      (using ^ : (Enumerator.Generator, Term.Name)): List[Enumerator] =
    val definition =
      Defn.Def(
        Nil,
        name,
        Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          Term.ParamClause(Term.Param(Nil, remaining, Some(\\("Int")), None)
                        :: Term.Param(Nil, acquire, Some(Type.Apply(\\("Option"), Type.ArgClause(Type.Apply(\\("Semaphore"), Type.ArgClause(\\("F") :: Nil)) :: Nil))), None) :: Nil)
       :: Term.ParamClause(Term.Param(Mod.Using() :: Nil,
                                      Name.Anonymous(), Some(\\("String")),
                                      None) :: Nil) :: Nil
        ) :: Nil,
        `: \\[F, Unit]`,
        `for * yield ()`(`* <- Semaphore(…)`(release, 0)
                      :: ^._1
                      :: `_ <- *`(Term.Apply(Term.Select(
                                               `for * yield ()`(`* <- *`(parameter -> Term.Apply(replication,
                                                                                                 Term.ArgClause(\(deferred) :: \(cbarrier) :: \(acquire) :: \(release) :: Nil))) :: `given String = ^._2` :: sum*),
                                               "concurrently"),
                                             Term.ArgClause(Term.If(Term.ApplyInfix(\(remaining), \("=="), Type.ArgClause(Nil), Term.ArgClause(Lit.Int(1) :: Nil)),
                                                                    Term.Select(\, "empty"),
                                                                    Term.Apply(Term.Apply(\(name),
                                                                                          Term.ArgClause(Term.ApplyInfix(\(remaining), \("-"), Type.ArgClause(Nil), Term.ArgClause(Lit.Int(1) :: Nil))
                                                                                                           :: Term.Apply(\("Some"), Term.ArgClause(\(release) :: Nil)) :: Nil)),
                                                                               Term.ArgClause(^._2 :: Nil, Some(Mod.Using())))) :: Nil)))*)
      )

    `* <- Stream.eval(*)`(deferred, `*[F]`("Deferred", \\("Boolean"))) ::
    `* <- Stream.eval(*)`(cbarrier, Term.Apply(`*[F]`("CyclicBarrier"), Term.ArgClause(Lit.Int(parallelism) :: Nil))) ::
    `* <- *`(name -> Term.Apply(Term.Select(\, \\), Term.ArgClause(Term.Block(definition :: \(name) :: Nil) :: Nil))) ::
    `_ <- *`(Term.Apply(\(name), Term.ArgClause(Lit.Int(parallelism) :: \("None") :: Nil))) :: Nil

  def `* <- +`(parameter: String, parallelism: Int, replication: Term, sum: List[Enumerator])
              (using id: => String, ^ : (Enumerator.Generator, Term.Name)): List[Enumerator] =
    `* <- +`(parameter, parallelism, id, id, id, id, id, id, replication, sum)
