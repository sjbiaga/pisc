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

package pisc
package emitter
package zs

import scala.meta.*
import dialects.Scala3

import parser.Calculus.`(*)`


object Meta extends emitter.shared.effects.Meta:

  override protected lazy val \ = "ZStream"

  val `: ZStream[Any, Throwable, Unit]` =
    Some(Type.Apply(\\(\), Type.ArgClause(\\("Any") :: \\("Throwable") :: \\("Unit") :: Nil)))

  val `: ZIO[Any, Throwable, Unit]` =
    Some(Type.Apply(\\("ZIO"), Type.ArgClause(\\("Any") :: \\("Throwable") :: \\("Unit") :: Nil)))


  def defn(body: Term): `(*)` => Defn.Def =
    case `(*)`("Main", _) =>
      Defn.Def(Nil,
               "Main", `String*`("args"), `: ZStream[Any, Throwable, Unit]`,
               body)
    case `(*)`(identifier, _, _params*) =>
      val params = _params.map(_.asSymbol.name)
      Defn.Def(Nil,
               identifier, `(…)`(params*), `: ZStream[Any, Throwable, Unit]`,
               body)


  def `String*`(* : String) =
    Member.ParamClauseGroup(
      Type.ParamClause(Nil),
      Term.ParamClause(Term.Param(Nil, *, Some(Type.Repeated(\\("String"))), None) :: Nil,
                       None) :: Nil
    ) :: Nil

  def `(…)`(* : String*) =
    Member.ParamClauseGroup(
      Type.ParamClause(Nil),
      Term.ParamClause(*
                        .map(Term.Param(Nil, _, Some(\\("()")), None))
                        .toList,
                       None) :: Nil
    ) :: Nil


  def `* <- ZStream.fromZIO(*)`(* : (String, Term)): Enumerator.Generator =
    `* <- *`(*._1 -> Term.Apply(Term.Select(\, "fromZIO"), Term.ArgClause(*._2 :: Nil)))

  def `_ <- ZStream.fromZIO(*)`(* : Term): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), Term.Apply(Term.Select(\, "fromZIO"), Term.ArgClause(* :: Nil)))

  private val `ZStream.fromZIO`: Term => Boolean =
    case Term.Select(Term.Name(`\\`), Term.Name("fromZIO")) => true
    case Term.Apply(it, _) => `ZStream.fromZIO`(it)
    case Term.ApplyType(it, _) => `ZStream.fromZIO`(it)
    case _ => false

  def `ZStream.fromZIO(…)`(`…`: List[Enumerator]): List[Enumerator] =
    `…`.map {
      case it @ Enumerator.Generator(_, rhs) if `ZStream.fromZIO`(rhs) => it
      case it: Enumerator.Generator => it.copy(rhs = Term.Apply(Term.Select(\, "fromZIO"), Term.ArgClause(it.rhs :: Nil)))
      case it => it
    }

  def `ZStream.fromZIO(Scope.make)(…).provideLayer(*)`(`…`: Term, * : String) =
    Term.Apply(Term.Select(Term.Apply(Term.Select(\, "fromZIO"),
                                      Term.ArgClause(Term.Select("Scope", "make") :: Nil)),
                           "flatMap"),
               Term.ArgClause(Term.Block(Term.Function(Term.ParamClause(Term.Param(Nil, *, None, None) :: Nil),
                                                       Term.Apply(Term.Select(Term.Apply(Term.Select(\, "fromZIO"),
                                                                                         Term.ArgClause(`…` :: Nil)),
                                                                              "provideLayer"),
                                                                  Term.ArgClause(Term.Apply(Term.Select("ZLayer", "succeed"),
                                                                                            Term.ArgClause(* :: Nil)) :: Nil))) :: Nil) :: Nil))

  def `*.close(Exit.unit)`(* : String) =
    `_ <- ZStream.fromZIO(*)`(Term.Apply(Term.Select(*, "close"), Term.ArgClause(Term.Select("Exit", "unit") :: Nil)))

  def `* <- Semaphore.make(…)`(* : String, `…`: Int = 1): Enumerator.Generator =
    `* <- ZStream.fromZIO(*)`(* -> Term.Apply(Term.Select("Semaphore", "make"),
                                              Term.ArgClause(Lit.Int(`…`) :: Nil)))

  def `* <- ….runLast; _ <- …`(* : String, `…`: (Term, Term)) =
    `…`._1 match
      case it: Term.ForYield =>
        `* <- *`(* -> Term.Select(it.copy(body = *), "runLast")) ::
        `_ <- *`(`if * then … else …`(Term.ApplyInfix(\(*), \("eq"), Type.ArgClause(Nil), Term.ArgClause(\("None") :: Nil)),
                                      Term.Select("ZIO", "unit"),
                                      `…`._2))

  def `*.withPermitScoped`(* : String): Enumerator.Generator =
    `_ <- *`(Term.Select(*, "withPermitScoped"))

  def `*.tryWithPermit(…)`(* : String, `…`: Term): Term =
    Term.Apply(Term.Select(*, "tryWithPermit"), Term.ArgClause(Term.Select(`…`, "runDrain") :: Nil))


  def `List( *, … ).collectAllPar`(* : Term*): Term =
    *.flatMap {
      case Term.Select(Term.Name(`\\`), Term.Name("unit")) => None
      case it => Some(it)
    } match
      case Nil => \(Nil)
      case it => Term.Select(Term.Apply(\("πLs"), Term.ArgClause(it.toList)), "πcollectAllPar")

  def `List( *, … ).collectAllParZIO`(* : Term*): Term =
    *.flatMap {
      case Term.Select(Term.Name(`\\`), Term.Name("unit")) => None
      case it => Some(it)
    } match
      case Nil => \(Nil)
      case it => Term.Select(Term.Apply(\("πLs"), Term.ArgClause(it.toList)), "πcollectAllParZIO")

  def `List( *, … ).collectAllPar(…)`(* : Term*)(`…`: String): Term =
    *.flatMap {
      case Term.Select(Term.Name(`\\`), Term.Name("unit")) => None
      case it => Some(it)
    } match
      case Nil => \(Nil)
      case it => Term.Apply(Term.Select(Term.Apply(\("πLs"), Term.ArgClause(it.toList)), "πcollectAllPar"),
                            Term.ArgClause(`…` :: Nil))


  def `\\.succeed { def *(*: Scope.Closeable, *: ()): ZIO[Any, Throwable, Unit] = …; * }`(* : ((String, String), String), `…`: Term): Term =
    Term.Apply(Term.Select(\, "succeed"),
               Term.ArgClause(
                 Term.Block(
                   Defn.Def(Nil,
                            *._1._1,
                            Member.ParamClauseGroup(Type.ParamClause(Nil),
                                                    Term.ParamClause(Term.Param(Nil,
                                                                                *._1._2,
                                                                                Some(Type.Select("Scope", \\("Closeable"))),
                                                                                None)
                                                                  :: Term.Param(Nil,
                                                                                *._2,
                                                                                Some(\\("()")),
                                                                                None)
                                                                  :: Nil, None) :: Nil) :: Nil,
                            `: ZIO[Any, Throwable, Unit]`,
                             `…`
                   ) :: \(*._1._1) :: Nil
                 ) :: Nil
               )
    )

  def `\\.succeed { def *(*: Scope.Closeable): ZIO[Any, Throwable, Unit] = …; * }`(* : (String, String), `…`: Term): Term =
    Term.Apply(Term.Select(\, "succeed"),
               Term.ArgClause(
                 Term.Block(
                   Defn.Def(Nil,
                            *._1,
                            Member.ParamClauseGroup(Type.ParamClause(Nil),
                                                    Term.ParamClause(Term.Param(Nil,
                                                                                *._2,
                                                                                Some(Type.Select("Scope", \\("Closeable"))),
                                                                                None)
                                                                  :: Nil, None) :: Nil) :: Nil,
                            `: ZIO[Any, Throwable, Unit]`,
                             `…`
                   ) :: \(*._1) :: Nil
                 ) :: Nil
               )
    )

  def `\\.succeed { def *(*: ()): ZStream[Any, Throwable, Unit] = …; * }`(* : (String, String), `…`: Term): Term =
    Term.Apply(Term.Select(\, "succeed"),
               Term.ArgClause(
                 Term.Block(
                   Defn.Def(Nil,
                            *._1,
                            Member.ParamClauseGroup(Type.ParamClause(Nil),
                                                    Term.ParamClause(Term.Param(Nil,
                                                                                *._2,
                                                                                Some(\\("()")),
                                                                                None) :: Nil, None) :: Nil) :: Nil,
                            `: ZStream[Any, Throwable, Unit]`,
                             `…`
                   ) :: \(*._1) :: Nil
                 ) :: Nil
               )
    )

  def `\\.succeed { lazy val *: ZStream[Any, Throwable, Unit] = …; * }`(* : String, `…`: Term): Term =
    Term.Apply(Term.Select(\, "succeed"),
               Term.ArgClause(Term.Block(
                                Defn.Val(Mod.Lazy() :: Nil,
                                         `* <- …`(*) :: Nil,
                                         `: ZStream[Any, Throwable, Unit]`,
                                         `…`
                                ) :: \(*) :: Nil
                              ) :: Nil
               )
    )


  private def `_ <- +`(parallelism: Int,
                       cbarrier: String,
                       name: String,
                       remaining: String,
                       take: String,
                       offer: String,
                       replication: Term,
                       sum: List[Enumerator]): List[Enumerator] =
    val definition =
      Defn.Def(
        Nil,
        name,
        Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          Term.ParamClause(Term.Param(Nil, remaining, Some(\\("Int")), None)
                        :: Term.Param(Nil, take, Some(Type.Apply(\\("Option"), Type.ArgClause(Type.Apply(\\("Queue"), Type.ArgClause(\\("Unit") :: Nil)) :: Nil))), None) :: Nil) :: Nil
        ) :: Nil,
        `: ZStream[Any, Throwable, Unit]`,
        Term.If(Term.ApplyInfix(\(remaining), \("=="), Type.ArgClause(Nil), Term.ArgClause(Lit.Int(1) :: Nil)),
                `for * yield ()`(`_ <- *`(Term.Apply(replication, Term.ArgClause(\(cbarrier) :: \(take) :: \("None") :: Nil))) :: sum*),
                `for * yield ()`(`* <- ZStream.fromZIO(*)`(offer,
                                                           Term.Apply(Term.ApplyType(Term.Select("Queue", "bounded"),
                                                                                     Type.ArgClause(\\("Unit") :: Nil)),
                                                                      Term.ArgClause(Lit.Int(1) :: Nil)))
                              :: `_ <- *`(Term.Apply(Term.Select(
                                                       `for * yield ()`(`_ <- *`(Term.Apply(replication,
                                                                                            Term.ArgClause(\(cbarrier) :: \(take) :: Term.Apply(\("Some"), Term.ArgClause(\(offer) :: Nil)) :: Nil))) :: sum*),
                                                       "drainFork"),
                                                     Term.ArgClause(Term.Apply(\(name),
                                                                               Term.ArgClause(Term.ApplyInfix(\(remaining), \("-"), Type.ArgClause(Nil), Term.ArgClause(Lit.Int(1) :: Nil))
                                                                                           :: Term.Apply(\("Some"), Term.ArgClause(\(offer) :: Nil)) :: Nil)) :: Nil)))*)
        )
      )

    `* <- ZStream.fromZIO(*)`(cbarrier, Term.Apply(Term.Select("CyclicBarrier", "make"), Term.ArgClause(Lit.Int(parallelism) :: Nil))) ::
    `* <- *`(name -> Term.Apply(Term.Select(\, "succeed"), Term.ArgClause(Term.Block(definition :: \(name) :: Nil) :: Nil))) ::
    `_ <- *`(Term.Apply(\(name), Term.ArgClause(Lit.Int(parallelism) :: \("None") :: Nil))) :: Nil

  def `_ <- +`(parallelism: Int, replication: Term, sum: List[Enumerator])(using id: => String): List[Enumerator] =
    `_ <- +`(parallelism, id, id, id, id, id, replication, sum)


  private def `* <- +`(parameter: String,
                       parallelism: Int,
                       cbarrier: String,
                       name: String,
                       remaining: String,
                       take: String,
                       offer: String,
                       replication: Term,
                       sum: List[Enumerator]): List[Enumerator] =
    val definition =
      Defn.Def(
        Nil,
        name,
        Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          Term.ParamClause(Term.Param(Nil, remaining, Some(\\("Int")), None)
                        :: Term.Param(Nil, take, Some(Type.Apply(\\("Option"), Type.ArgClause(Type.Apply(\\("Queue"), Type.ArgClause(\\("Unit") :: Nil)) :: Nil))), None) :: Nil) :: Nil
        ) :: Nil,
        `: ZStream[Any, Throwable, Unit]`,
        Term.If(Term.ApplyInfix(\(remaining), \("=="), Type.ArgClause(Nil), Term.ArgClause(Lit.Int(1) :: Nil)),
                `for * yield ()`(`* <- *`(parameter -> Term.Apply(replication, Term.ArgClause(\(cbarrier) :: \(take) :: \("None") :: Nil))) :: sum*),
                `for * yield ()`(`* <- ZStream.fromZIO(*)`(offer,
                                                           Term.Apply(Term.ApplyType(Term.Select("Queue", "bounded"),
                                                                                     Type.ArgClause(\\("Unit") :: Nil)),
                                                                      Term.ArgClause(Lit.Int(1) :: Nil)))
                              :: `_ <- *`(Term.Apply(Term.Select(
                                                       `for * yield ()`(`* <- *`(parameter -> Term.Apply(replication,
                                                                                                         Term.ArgClause(\(cbarrier) :: \(take) :: Term.Apply(\("Some"), Term.ArgClause(\(offer) :: Nil)) :: Nil))) :: sum*),
                                                       "drainFork"),
                                                     Term.ArgClause(Term.Apply(\(name),
                                                                               Term.ArgClause(Term.ApplyInfix(\(remaining), \("-"), Type.ArgClause(Nil), Term.ArgClause(Lit.Int(1) :: Nil))
                                                                                           :: Term.Apply(\("Some"), Term.ArgClause(\(offer) :: Nil)) :: Nil)) :: Nil)))*)
        )
      )

    `* <- ZStream.fromZIO(*)`(cbarrier, Term.Apply(Term.Select("CyclicBarrier", "make"), Term.ArgClause(Lit.Int(parallelism) :: Nil))) ::
    `* <- *`(name -> Term.Apply(Term.Select(\, "succeed"), Term.ArgClause(Term.Block(definition :: \(name) :: Nil) :: Nil))) ::
    `_ <- *`(Term.Apply(\(name), Term.ArgClause(Lit.Int(parallelism) :: \("None") :: Nil))) :: Nil

  def `* <- +`(parameter: String, parallelism: Int, replication: Term, sum: List[Enumerator])(using id: => String): List[Enumerator] =
    `* <- +`(parameter, parallelism, id, id, id, id, id, replication, sum)
