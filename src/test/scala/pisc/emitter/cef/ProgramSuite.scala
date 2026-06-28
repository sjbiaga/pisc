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
package cef

import scala.meta.*
import dialects.Scala3

import munit.FunSuite

import parser.Calculus.*
import Meta.{ \ => _, * }
import Program.emit
import ProgramSuite.*
import ProgramSuite.given


class ProgramSuite extends FunSuite:

  // REPLICATION (BOUND OUTPUT) ////////////////////////////////////////////////

  /**
    * `! .guard<νname>.`
    *
    * @example {{{
    * for {
    *   _υ2υ <- IO {
    *     def _υ2υ(name: `()`): String => IO[Any] = { implicit ^ =>
    *       πLs(
    *         for {
    *           name <- ν
    *           _    <- ???.flatMap { _υ3υ =>
    *             if (_υ3υ eq null)
    *               IO.cede
    *             else
    *               for {
    *                 _    <- IO.unit
    *                 _υ1υ <- `π-uuid`
    *                 _    <- _υ2υ(name)(_υ1υ)
    *               } yield ()
    *           }
    *         } yield ()
    *       ).πparSequence
    *     }
    *     _υ2υ
    *   }
    *   _    <-
    *     for {
    *       name <- ν
    *       _    <- ???.flatMap { _υ3υ =>
    *         if (_υ3υ eq null)
    *           IO.cede
    *         else
    *           for {
    *             _    <- IO.unit
    *             _υ1υ <- `π-uuid`
    *             _    <- _υ2υ(name)(_υ1υ)
    *           } yield ()
    *       }
    *     } yield ()
    * } yield ()
    * }}}
    */
  test("replication - bound output guard - no parallelism & no pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(-1, None, Some(π(λ(Symbol("guard")), λ(Symbol("name")), Some("ν"), Some(-1), None)(id)), `+`(-1))

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Def(Nil,
                                                           Term.Name(_),
                                                           Nil,
                                                           List(Term.Param(Nil, Term.Name("name"), Some(Type.Name("()")), None) :: Nil),
                                                           Some(Type.Function(Type.Name("String") :: Nil, Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil))),
                                                           Term.Block(
                                                             Term.Function(Term.Param(Mod.Implicit() :: Nil, Term.Name("^"), None, None) :: Nil,
                                                                           Term.Select(
                                                                             Term.Apply(
                                                                               Term.Name("πLs"),
                                                                               List(
                                                                                 Term.ForYield(List(
                                                                                                 Enumerator.Generator(Pat.Var(Term.Name("name")), Term.Name("ν")),
                                                                                                 Enumerator.Generator(Pat.Wildcard(),
                                                                                                                      Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                                                                                                 Term.Block(Term.Function(_,
                                                                                                                                                          Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                                                                                                  Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                                                                                  Term.ForYield(List(
                                                                                                                                                                                  Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(`\\`), Term.Name("unit"))),
                                                                                                                                                                                  Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                                                                                                  Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                                                                                                                ),
                                                                                                                                                                                Lit.Unit())
                                                                                                                                                          )
                                                                                                                                           ) :: Nil
                                                                                                                                 ) :: Nil))
                                                                                               ),
                                                                                               Lit.Unit())
                                                                               )
                                                                             ),
                                                                             Term.Name("πparSequence")
                                                                           )
                                                             ) :: Nil
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.ForYield(List(
                                                     Enumerator.Generator(Pat.Var(Term.Name("name")), Term.Name("ν")),
                                                     Enumerator.Generator(Pat.Wildcard(),
                                                                          Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                                                     Term.Block(Term.Function(_,
                                                                                                              Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                                                      Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                                      Term.ForYield(List(
                                                                                                                                      Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(`\\`), Term.Name("unit"))),
                                                                                                                                      Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                                                      Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                                                                    ),
                                                                                                                                    Lit.Unit())
                                                                                                              )
                                                                                               ) :: Nil
                                                                                     ) :: Nil))
                                                   ),
                                                   Lit.Unit())
                )
      ) => true
    }
  }

  /**
    * `! 13* .guard<νname>.`
    *
    * @example {{{
    * for {
    *   _υ3υ <- Semaphore[IO](13)
    *   _υ2υ <- IO {
    *     def _υ2υ(name: `()`): String => IO[Any] = { implicit ^ =>
    *       πLs(
    *         _υ3υ.release,
    *         for {
    *           name <- ν
    *           _    <- ???.flatMap { _υ4υ =>
    *             if (_υ4υ eq null)
    *               IO.cede
    *             else
    *               for {
    *                 _    <- IO.unit
    *                 _    <- _υ3υ.acquire
    *                 _υ1υ <- `π-uuid`
    *                 _    <- _υ2υ(name)(_υ1υ)
    *               } yield ()
    *           }
    *         } yield ()
    *       ).πparSequence
    *     }
    *     _υ2υ
    *   }
    *   _    <-
    *     for {
    *       name <- ν
    *       _    <- ???.flatMap { _υ4υ =>
    *         if (_υ4υ eq null)
    *           IO.cede
    *         else
    *           for {
    *             _    <- IO.unit
    *             _    <- _υ3υ.acquire
    *             _υ1υ <- `π-uuid`
    *             _    <- _υ2υ(name)(_υ1υ)
    *           } yield ()
    *       }
    *     } yield ()
    * } yield ()
    * }}}
    */
  test("replication - bound output guard - parallelism >= 0 & no pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(13, None, Some(π(λ(Symbol("guard")), λ(Symbol("name")), Some("ν"), Some(-1), None)(id)), `+`(-1))

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Apply(Term.ApplyType(Term.Name("Semaphore"),
                                                                                      Type.Name(`\\`) :: Nil),
                                                                       Lit.Int(13) :: Nil)),
                Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Def(Nil,
                                                           Term.Name(_),
                                                           Nil,
                                                           List(Term.Param(Nil, Term.Name("name"), Some(Type.Name("()")), None) :: Nil),
                                                           Some(Type.Function(Type.Name("String") :: Nil, Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil))),
                                                           Term.Block(
                                                             Term.Function(Term.Param(Mod.Implicit() :: Nil, Term.Name("^"), None, None) :: Nil,
                                                                           Term.Select(
                                                                             Term.Apply(
                                                                               Term.Name("πLs"),
                                                                               List(
                                                                                 Term.Select(Term.Name(_), Term.Name("release")),
                                                                                 Term.ForYield(List(
                                                                                                 Enumerator.Generator(Pat.Var(Term.Name("name")), Term.Name("ν")),
                                                                                                 Enumerator.Generator(Pat.Wildcard(),
                                                                                                                      Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                                                                                                 Term.Block(Term.Function(_,
                                                                                                                                                          Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                                                                                                  Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                                                                                  Term.ForYield(List(
                                                                                                                                                                                  Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(`\\`), Term.Name("unit"))),
                                                                                                                                                                                  Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                                                                                                                  Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                                                                                                  Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                                                                                                                ),
                                                                                                                                                                                Lit.Unit())
                                                                                                                                                          )
                                                                                                                                           ) :: Nil
                                                                                                                                 ) :: Nil))
                                                                                               ),
                                                                                               Lit.Unit())
                                                                               )
                                                                             ),
                                                                             Term.Name("πparSequence")
                                                                           )
                                                             ) :: Nil
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.ForYield(List(
                                                     Enumerator.Generator(Pat.Var(Term.Name("name")), Term.Name("ν")),
                                                     Enumerator.Generator(Pat.Wildcard(),
                                                                          Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                                                     Term.Block(Term.Function(_,
                                                                                                              Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                                                      Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                                      Term.ForYield(List(
                                                                                                                                      Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(`\\`), Term.Name("unit"))),
                                                                                                                                      Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                                                                      Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                                                      Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                                                                    ),
                                                                                                                                    Lit.Unit())
                                                                                                              )
                                                                                               ) :: Nil
                                                                                     ) :: Nil))
                                                   ),
                                                   Lit.Unit())
                )
      ) => true
    }
  }

  /**
    * `! 13,seconds .guard<νname>.`
    *
    * @example {{{
    * for {
    *   _υ2υ <- IO {
    *     def _υ2υ(name: `()`): String => IO[Any] = { implicit ^ =>
    *       πLs(
    *         for {
    *           _ <- IO.sleep(13L.seconds)
    *           _ <-
    *             for {
    *               name <- ν
    *               _    <- ???.flatMap { _υ3υ =>
    *                 if (_υ3υ eq null)
    *                   IO.cede
    *                 else
    *                   for {
    *                     _    <- IO.unit
    *                     _υ1υ <- `π-uuid`
    *                     _    <- _υ2υ(name)(_υ1υ)
    *                   } yield ()
    *               }
    *             } yield ()
    *         } yield ()
    *       ).πparSequence
    *     }
    *     _υ2υ
    *   }
    *   _    <-
    *     for {
    *       name <- ν
    *       _    <- ???.flatMap { _υ3υ =>
    *         if (_υ3υ eq null)
    *           IO.cede
    *         else
    *           for {
    *             _    <- IO.unit
    *             _υ1υ <- `π-uuid`
    *             _    <- _υ2υ(name)(_υ1υ)
    *           } yield ()
    *       }
    *     } yield ()
    * } yield ()
    * }}}
    */
  test("replication - bound output guard - no parallelism & nonempty pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(-1, Some(13L->"seconds"), Some(π(λ(Symbol("guard")), λ(Symbol("name")), Some("ν"), Some(-1), None)(id)), `+`(-1))

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Def(Nil,
                                                           Term.Name(_),
                                                           Nil,
                                                           List(Term.Param(Nil, Term.Name("name"), Some(Type.Name("()")), None) :: Nil),
                                                           Some(Type.Function(Type.Name("String") :: Nil, Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil))),
                                                           Term.Block(
                                                             Term.Function(Term.Param(Mod.Implicit() :: Nil, Term.Name("^"), None, None) :: Nil,
                                                                           Term.Select(
                                                                             Term.Apply(
                                                                               Term.Name("πLs"),
                                                                               List(
                                                                                 Term.ForYield(List(
                                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("sleep")),
                                                                                                                                                 Term.Select(Lit.Long(13L), Term.Name("seconds")) :: Nil)),
                                                                                                 Enumerator.Generator(Pat.Wildcard(),
                                                                                                                      Term.ForYield(List(
                                                                                                                                      Enumerator.Generator(Pat.Var(Term.Name("name")), Term.Name("ν")),
                                                                                                                                      Enumerator.Generator(Pat.Wildcard(),
                                                                                                                                                           Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                                                                                                                                      Term.Block(Term.Function(_,
                                                                                                                                                                                               Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                                                                                                                                       Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                                                                                                                       Term.ForYield(List(
                                                                                                                                                                                                                       Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(`\\`), Term.Name("unit"))),
                                                                                                                                                                                                                       Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                                                                                                                                       Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                                                                                                                                                     ),
                                                                                                                                                                                                                     Lit.Unit())
                                                                                                                                                                                               )
                                                                                                                                                                                ) :: Nil
                                                                                                                                                                      ) :: Nil))
                                                                                                                                    ),
                                                                                                                                    Lit.Unit())
                                                                                                 )
                                                                                               ),
                                                                                               Lit.Unit())
                                                                               )
                                                                             ),
                                                                             Term.Name("πparSequence")
                                                                           )
                                                             ) :: Nil
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.ForYield(List(
                                                     Enumerator.Generator(Pat.Var(Term.Name("name")), Term.Name("ν")),
                                                     Enumerator.Generator(Pat.Wildcard(),
                                                                          Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                                                     Term.Block(Term.Function(_,
                                                                                                              Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                                                      Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                                      Term.ForYield(List(
                                                                                                                                      Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(`\\`), Term.Name("unit"))),
                                                                                                                                      Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                                                      Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                                                                    ),
                                                                                                                                    Lit.Unit())
                                                                                                              )
                                                                                               ) :: Nil
                                                                                     ) :: Nil))
                                                   ),
                                                   Lit.Unit())
                )
      ) => true
    }
  }

  /**
    * `! 13* 13,seconds .guard<νname>.`
    *
    * @example {{{
    * for {
    *   _υ3υ <- Semaphore[IO](13)
    *   _υ2υ <- IO {
    *     def _υ2υ(name: `()`): String => IO[Any] = { implicit ^ =>
    *       πLs(
    *         _υ3υ.release,
    *         for {
    *           _ <- IO.sleep(13L.seconds)
    *           _ <-
    *             for {
    *               name <- ν
    *               _    <- ???.flatMap { _υ4υ =>
    *                 if (_υ4υ eq null)
    *                   IO.cede
    *                 else
    *                   for {
    *                     _    <- IO.unit
    *                     _    <- _υ3υ.acquire
    *                     _υ1υ <- `π-uuid`
    *                     _    <- _υ2υ(name)(_υ1υ)
    *                   } yield ()
    *               }
    *             } yield ()
    *         } yield ()
    *       ).πparSequence
    *     }
    *     _υ2υ
    *   }
    *   _    <-
    *     for {
    *       name <- ν
    *       _    <- ???.flatMap { _υ4υ =>
    *         if (_υ4υ eq null)
    *           IO.cede
    *         else
    *           for {
    *             _    <- IO.unit
    *             _    <- _υ3υ.acquire
    *             _υ1υ <- `π-uuid`
    *             _    <- _υ2υ(name)(_υ1υ)
    *           } yield ()
    *       }
    *     } yield ()
    * } yield ()
    * }}}
    */
  test("replication - bound output guard - parallelism >= 0 & nonempty pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(13, Some(13L->"seconds"), Some(π(λ(Symbol("guard")), λ(Symbol("name")), Some("ν"), Some(-1), None)(id)), `+`(-1))

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Apply(Term.ApplyType(Term.Name("Semaphore"),
                                                                                      Type.Name(`\\`) :: Nil),
                                                                       Lit.Int(13) :: Nil)),
                Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Def(Nil,
                                                           Term.Name(_),
                                                           Nil,
                                                           List(Term.Param(Nil, Term.Name("name"), Some(Type.Name("()")), None) :: Nil),
                                                           Some(Type.Function(Type.Name("String") :: Nil, Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil))),
                                                           Term.Block(
                                                             Term.Function(Term.Param(Mod.Implicit() :: Nil, Term.Name("^"), None, None) :: Nil,
                                                                           Term.Select(
                                                                             Term.Apply(
                                                                               Term.Name("πLs"),
                                                                               List(
                                                                                 Term.Select(Term.Name(_), Term.Name("release")),
                                                                                 Term.ForYield(List(
                                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("sleep")),
                                                                                                                                                 Term.Select(Lit.Long(13L), Term.Name("seconds")) :: Nil)),
                                                                                                 Enumerator.Generator(Pat.Wildcard(),
                                                                                                                      Term.ForYield(List(
                                                                                                                                      Enumerator.Generator(Pat.Var(Term.Name("name")), Term.Name("ν")),
                                                                                                                                      Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                                                                                                                                                      Term.Block(Term.Function(_,
                                                                                                                                                                                                               Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                                                                                                                                                       Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                                                                                                                                       Term.ForYield(List(
                                                                                                                                                                                                                                       Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(`\\`), Term.Name("unit"))),
                                                                                                                                                                                                                                       Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                                                                                                                                                                       Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                                                                                                                                                       Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                                                                                                                                                                     ),
                                                                                                                                                                                                                                     Lit.Unit())
                                                                                                                                                                                                               )
                                                                                                                                                                                                ) :: Nil
                                                                                                                                                                                      ) :: Nil))
                                                                                                                                    ),
                                                                                                                                    Lit.Unit())
                                                                                                 )
                                                                                               ),
                                                                                               Lit.Unit())
                                                                               )
                                                                             ),
                                                                             Term.Name("πparSequence")
                                                                           )
                                                             ) :: Nil
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.ForYield(List(
                                                     Enumerator.Generator(Pat.Var(Term.Name("name")), Term.Name("ν")),
                                                     Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                                                                     Term.Block(Term.Function(_,
                                                                                                                              Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                                                                      Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                                                      Term.ForYield(List(
                                                                                                                                                      Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(`\\`), Term.Name("unit"))),
                                                                                                                                                      Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                                                                                      Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                                                                      Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                                                                                    ),
                                                                                                                                                    Lit.Unit())
                                                                                                                              )
                                                                                                               ) :: Nil
                                                                                                     ) :: Nil))
                                                   ),
                                                   Lit.Unit())
                )
      ) => true
    }
  }

  //////////////////////////////////////////////// replication (bound output) //

  // REPLICATION (INPUT) ///////////////////////////////////////////////////////

  /**
    * `! 13* .guard(name).`
    *
    * @example {{{
    * for {
    *   _υ2υ <- IO {
    *     def _υ2υ(name: `()`): String => IO[Any] = { implicit ^ =>
    *       πLs(
    *         ???.flatMap { (name, _υ3υ) =>
    *           if (_υ3υ eq null)
    *             IO.cede
    *           else
    *             for {
    *               _υ1υ <- `π-uuid`
    *               _    <- _υ2υ(name)(_υ1υ)
    *             } yield ()
    *         }
    *       ).πparSequence
    *     }
    *     _υ2υ
    *   }
    *   _    <- ???.flatMap { (name, _υ3υ) =>
    *     if (_υ3υ eq null)
    *       IO.cede
    *     else
    *       for {
    *         _υ1υ <- `π-uuid`
    *         _    <- _υ2υ(name)(_υ1υ)
    *       } yield ()
    *   }
    * } yield ()
    * }}}
    */
  test("replication - input guard - no parallelism & no pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(-1, None, Some(π(λ(Symbol("guard")), λ(Symbol("name")), Some(""), Some(-1), None)(id)), `+`(-1))

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Def(Nil,
                                                           Term.Name(_),
                                                           Nil,
                                                           List(Term.Param(Nil, Term.Name("name"), Some(Type.Name("()")), None) :: Nil),
                                                           Some(Type.Function(Type.Name("String") :: Nil, Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil))),
                                                           Term.Block(
                                                             Term.Function(Term.Param(Mod.Implicit() :: Nil, Term.Name("^"), None, None) :: Nil,
                                                                           Term.Block(
                                                                             Term.Select(
                                                                               Term.Apply(
                                                                                 Term.Name("πLs"),
                                                                                 List(
                                                                                   Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                                                              Term.Block(Term.Function(_,
                                                                                                                       Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                                                               Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                                               Term.ForYield(List(
                                                                                                                                               Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                                                               Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                                                                             ),
                                                                                                                                             Lit.Unit())
                                                                                                                       )
                                                                                                        ) :: Nil
                                                                                              ) :: Nil)
                                                                                 )
                                                                               ),
                                                                               Term.Name("πparSequence")
                                                                             ) :: Nil)
                                                             ) :: Nil
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                Term.Block(Term.Function(_,
                                                                         Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                 Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                 Term.ForYield(List(
                                                                                                 Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                               ),
                                                                                               Lit.Unit())
                                                                         )
                                                          ) :: Nil
                                                ) :: Nil))
      ) => true
    }
  }

  /**
    * `! 13* .guard(name).`
    *
    * @example {{{
    * for {
    *   _υ3υ <- Semaphore[IO](13)
    *   _υ2υ <- IO {
    *     def _υ2υ(name: `()`): String => IO[Any] = { implicit ^ =>
    *       πLs(
    *         _υ3υ.release,
    *         ???.flatMap { (name, _υ4υ) =>
    *           if (_υ4υ eq null)
    *             IO.cede
    *           else
    *             for {
    *               _    <- _υ3υ.acquire
    *               _υ1υ <- `π-uuid`
    *               _    <- _υ2υ(name)(_υ1υ)
    *             } yield ()
    *         }
    *       ).πparSequence
    *     }
    *     _υ2υ
    *   }
    *   _    <- ???.flatMap { (name, _υ4υ) =>
    *     if (_υ4υ eq null)
    *       IO.cede
    *     else
    *       for {
    *         _    <- _υ3υ.acquire
    *         _υ1υ <- `π-uuid`
    *         _    <- _υ2υ(name)(_υ1υ)
    *       } yield ()
    *   }
    * } yield ()
    * }}}
    */
  test("replication - input guard - parallelism >= 0 & no pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(13, None, Some(π(λ(Symbol("guard")), λ(Symbol("name")), Some(""), Some(-1), None)(id)), `+`(-1))

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Apply(Term.ApplyType(Term.Name("Semaphore"),
                                                                                      Type.Name(`\\`) :: Nil),
                                                                       Lit.Int(13) :: Nil)),
                Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Def(Nil,
                                                           Term.Name(_),
                                                           Nil,
                                                           List(Term.Param(Nil, Term.Name("name"), Some(Type.Name("()")), None) :: Nil),
                                                           Some(Type.Function(Type.Name("String") :: Nil, Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil))),
                                                           Term.Block(
                                                             Term.Function(Term.Param(Mod.Implicit() :: Nil, Term.Name("^"), None, None) :: Nil,
                                                                           Term.Block(
                                                                             Term.Select(
                                                                               Term.Apply(
                                                                                 Term.Name("πLs"),
                                                                                 List(
                                                                                   Term.Select(Term.Name(_), Term.Name("release")),
                                                                                   Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                                                              Term.Block(Term.Function(_,
                                                                                                                       Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                                                               Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                                               Term.ForYield(List(
                                                                                                                                               Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                                                                               Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                                                               Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                                                                             ),
                                                                                                                                             Lit.Unit())
                                                                                                                       )
                                                                                                        ) :: Nil
                                                                                              ) :: Nil)
                                                                                 )
                                                                               ),
                                                                               Term.Name("πparSequence")
                                                                             ) :: Nil)
                                                             ) :: Nil
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                Term.Block(Term.Function(_,
                                                                         Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                 Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                 Term.ForYield(List(
                                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                                 Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                               ),
                                                                                               Lit.Unit())
                                                                         )
                                                          ) :: Nil
                                                ) :: Nil))
      ) => true
    }
  }

  /**
    * `! 13,seconds .guard(name).`
    *
    * @example {{{
    * for {
    *   _υ2υ <- IO {
    *     def _υ2υ(name: `()`): String => IO[Any] = { implicit ^ =>
    *       πLs(
    *         for {
    *           _ <- IO.sleep(13L.seconds)
    *           _ <- ???.flatMap { (name, _υ3υ) =>
    *             if (_υ3υ eq null)
    *               IO.cede
    *             else
    *               for {
    *                 _υ1υ <- `π-uuid`
    *                 _    <- _υ2υ(name)(_υ1υ)
    *               } yield ()
    *           }
    *         } yield ()
    *       ).πparSequence
    *     }
    *     _υ2υ
    *   }
    *   _    <- ???.flatMap { (name, _υ3υ) =>
    *     if (_υ3υ eq null)
    *       IO.cede
    *     else
    *       for {
    *         _υ1υ <- `π-uuid`
    *         _    <- _υ2υ(name)(_υ1υ)
    *       } yield ()
    *   }
    * } yield ()
    * }}}
    */
  test("replication - input guard - no parallelism & nonempty pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(-1, Some(13L->"seconds"), Some(π(λ(Symbol("guard")), λ(Symbol("name")), Some(""), Some(-1), None)(id)), `+`(-1))

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Def(Nil,
                                                           Term.Name(_),
                                                           Nil,
                                                           List(Term.Param(Nil, Term.Name("name"), Some(Type.Name("()")), None) :: Nil),
                                                           Some(Type.Function(Type.Name("String") :: Nil, Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil))),
                                                           Term.Block(
                                                             Term.Function(Term.Param(Mod.Implicit() :: Nil, Term.Name("^"), None, None) :: Nil,
                                                                           Term.Block(
                                                                             Term.Select(
                                                                               Term.Apply(
                                                                                 Term.Name("πLs"),
                                                                                 List(
                                                                                   Term.ForYield(List(
                                                                                                   Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("sleep")),
                                                                                                                                                   Term.Select(Lit.Long(13L), Term.Name("seconds")) :: Nil)),
                                                                                                   Enumerator.Generator(Pat.Wildcard(),
                                                                                                                        Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                                                                                                   Term.Block(Term.Function(_,
                                                                                                                                                            Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                                                                                                    Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                                                                                    Term.ForYield(List(
                                                                                                                                                                                    Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                                                                                                    Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                                                                                                                  ),
                                                                                                                                                                                  Lit.Unit())
                                                                                                                                                            )
                                                                                                                                             ) :: Nil
                                                                                                                                   ) :: Nil))
                                                                                                 ),
                                                                                                 Lit.Unit())
                                                                                 )
                                                                               ),
                                                                               Term.Name("πparSequence")
                                                                             ) :: Nil)
                                                             ) :: Nil
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                Term.Block(Term.Function(_,
                                                                         Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                 Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                 Term.ForYield(List(
                                                                                                 Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                               ),
                                                                                               Lit.Unit())
                                                                         )
                                                          ) :: Nil
                                                ) :: Nil))
      ) => true
    }
  }

  /**
    * `! 13* 13,seconds .guard(name).`
    *
    * @example {{{
    * for {
    *   _υ3υ <- Semaphore[IO](13)
    *   _υ2υ <- IO {
    *     def _υ2υ(name: `()`): String => IO[Any] = { implicit ^ =>
    *       πLs(
    *         _υ3υ.release,
    *         for {
    *           _ <- IO.sleep(13L.seconds)
    *           _ <- ???.flatMap { (name, _υ4υ) =>
    *             if (_υ4υ eq null)
    *               IO.cede
    *             else
    *               for {
    *                 _    <- _υ3υ.acquire
    *                 _υ1υ <- `π-uuid`
    *                 _    <- _υ2υ(name)(_υ1υ)
    *               } yield ()
    *           }
    *         } yield ()
    *       ).πparSequence
    *     }
    *     _υ2υ
    *   }
    *   _    <- ???.flatMap { (name, _υ4υ) =>
    *     if (_υ4υ eq null)
    *       IO.cede
    *     else
    *       for {
    *         _    <- _υ3υ.acquire
    *         _υ1υ <- `π-uuid`
    *         _    <- _υ2υ(name)(_υ1υ)
    *       } yield ()
    *   }
    * } yield ()
    * }}}
    */
  test("replication - input guard - parallelism >= 0 & nonempty pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(13, Some(13L->"seconds"), Some(π(λ(Symbol("guard")), λ(Symbol("name")), Some(""), Some(-1), None)(id)), `+`(-1))

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Apply(Term.ApplyType(Term.Name("Semaphore"),
                                                                                      Type.Name(`\\`) :: Nil),
                                                                       Lit.Int(13) :: Nil)),
                Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Def(Nil,
                                                           Term.Name(_),
                                                           Nil,
                                                           List(Term.Param(Nil, Term.Name("name"), Some(Type.Name("()")), None) :: Nil),
                                                           Some(Type.Function(Type.Name("String") :: Nil, Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil))),
                                                           Term.Block(
                                                             Term.Function(Term.Param(Mod.Implicit() :: Nil, Term.Name("^"), None, None) :: Nil,
                                                                           Term.Block(
                                                                             Term.Select(
                                                                               Term.Apply(
                                                                                 Term.Name("πLs"),
                                                                                 List(
                                                                                   Term.Select(Term.Name(_), Term.Name("release")),
                                                                                   Term.ForYield(List(
                                                                                                   Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("sleep")),
                                                                                                                                                   Term.Select(Lit.Long(13L), Term.Name("seconds")) :: Nil)),
                                                                                                   Enumerator.Generator(Pat.Wildcard(),
                                                                                                                        Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                                                                                                   Term.Block(Term.Function(_,
                                                                                                                                                            Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                                                                                                    Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                                                                                    Term.ForYield(List(
                                                                                                                                                                                    Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                                                                                                                    Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                                                                                                    Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                                                                                                                  ),
                                                                                                                                                                                  Lit.Unit())
                                                                                                                                                            )
                                                                                                                                             ) :: Nil
                                                                                                                                   ) :: Nil))
                                                                                                 ),
                                                                                                 Lit.Unit())
                                                                                 )
                                                                               ),
                                                                               Term.Name("πparSequence")
                                                                             ) :: Nil)
                                                             ) :: Nil
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                Term.Block(Term.Function(_,
                                                                         Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                 Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                 Term.ForYield(List(
                                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                                 Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                               ),
                                                                                               Lit.Unit())
                                                                         )
                                                          ) :: Nil
                                                ) :: Nil))
      ) => true
    }
  }

  /////////////////////////////////////////////////////// replication (input) //

  // REPLICATION (INPUT) (TYPED) ///////////////////////////////////////////////

  /**
    * `! .guard(name: Int /**/).`
    *
    * @example {{{
    * for {
    *   _υ3υ <- IO {
    *     def _υ3υ(_υ2υ: `()`): String => IO[Any] = { implicit ^ =>
    *       val name: Int = ???
    *       πLs(
    *         ???.flatMap { (name, _υ4υ) =>
    *           if (_υ4υ eq null)
    *             IO.cede
    *           else
    *             for {
    *               _υ1υ <- `π-uuid`
    *               _    <- _υ3υ(name)(_υ1υ)
    *             } yield ()
    *         }
    *       ).πparSequence
    *     }
    *     _υ3υ
    *   }
    *   _    <- ???.flatMap { (name, _υ4υ) =>
    *     if (_υ4υ eq null)
    *       IO.cede
    *     else
    *       for {
    *         _υ1υ <- `π-uuid`
    *         _    <- _υ3υ(name)(_υ1υ)
    *       } yield ()
    *   }
    * } yield ()
    * }}}
    */
  test("replication - input guard - typed - no parallelism & no pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(-1, None, Some(π(λ(Symbol("guard")), λ(Symbol("name"))(using Some(\\("Int")->None)), Some(""), Some(-1), None)(id)), `+`(-1))

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Def(Nil,
                                                           Term.Name(_),
                                                           Nil,
                                                           List(Term.Param(Nil, Term.Name(_), Some(Type.Name("()")), None) :: Nil),
                                                           Some(Type.Function(Type.Name("String") :: Nil, Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil))),
                                                           Term.Block(
                                                             Term.Function(Term.Param(Mod.Implicit() :: Nil, Term.Name("^"), None, None) :: Nil,
                                                                           Term.Block(
                                                                             Defn.Val(Nil, Pat.Var(Term.Name("name")) :: Nil, Some(Type.Name("Int")), _) ::
                                                                             Term.Select(
                                                                               Term.Apply(
                                                                                 Term.Name("πLs"),
                                                                                 List(
                                                                                   Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                                                              Term.Block(Term.Function(_,
                                                                                                                       Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                                                               Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                                               Term.ForYield(List(
                                                                                                                                               Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                                                               Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                                                                             ),
                                                                                                                                             Lit.Unit())
                                                                                                                       )
                                                                                                        ) :: Nil
                                                                                              ) :: Nil)
                                                                                 )
                                                                               ),
                                                                               Term.Name("πparSequence")
                                                                             ) :: Nil)
                                                             ) :: Nil
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                Term.Block(Term.Function(_,
                                                                         Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                 Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                 Term.ForYield(List(
                                                                                                 Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                               ),
                                                                                               Lit.Unit())
                                                                         )
                                                          ) :: Nil
                                                ) :: Nil))
      ) => true
    }
  }

  /**
    * `! 13* .guard(name: Int /**/).`
    *
    * @example {{{
    * for {
    *   _υ4υ <- Semaphore[IO](13)
    *   _υ3υ <- IO {
    *     def _υ3υ(_υ2υ: `()`): String => IO[Any] = { implicit ^ =>
    *       val name: Int = ???
    *       πLs(
    *         _υ4υ.release,
    *         ???.flatMap { (name, _υ5υ) =>
    *           if (_υ5υ eq null)
    *             IO.cede
    *           else
    *             for {
    *               _    <- _υ4υ.acquire
    *               _υ1υ <- `π-uuid`
    *               _    <- _υ3υ(name)(_υ1υ)
    *             } yield ()
    *         }
    *       ).πparSequence
    *     }
    *     _υ3υ
    *   }
    *   _    <- ???.flatMap { (name, _υ5υ) =>
    *     if (_υ5υ eq null)
    *       IO.cede
    *     else
    *       for {
    *         _    <- _υ4υ.acquire
    *         _υ1υ <- `π-uuid`
    *         _    <- _υ3υ(name)(_υ1υ)
    *       } yield ()
    *   }
    * } yield ()
    * }}}
    */
  test("replication - input guard - typed - parallelism >= 0 & no pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(13, None, Some(π(λ(Symbol("guard")), λ(Symbol("name"))(using Some(\\("Int")->None)), Some(""), Some(-1), None)(id)), `+`(-1))

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Apply(Term.ApplyType(Term.Name("Semaphore"),
                                                                                      Type.Name(`\\`) :: Nil),
                                                                       Lit.Int(13) :: Nil)),
                Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Def(Nil,
                                                           Term.Name(_),
                                                           Nil,
                                                           List(Term.Param(Nil, Term.Name(_), Some(Type.Name("()")), None) :: Nil),
                                                           Some(Type.Function(Type.Name("String") :: Nil, Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil))),
                                                           Term.Block(
                                                             Term.Function(Term.Param(Mod.Implicit() :: Nil, Term.Name("^"), None, None) :: Nil,
                                                                           Term.Block(
                                                                             Defn.Val(Nil, Pat.Var(Term.Name("name")) :: Nil, Some(Type.Name("Int")), _) ::
                                                                             Term.Select(
                                                                               Term.Apply(
                                                                                 Term.Name("πLs"),
                                                                                 List(
                                                                                   Term.Select(Term.Name(_), Term.Name("release")),
                                                                                   Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                                                              Term.Block(Term.Function(_,
                                                                                                                       Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                                                               Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                                               Term.ForYield(List(
                                                                                                                                               Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                                                                               Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                                                               Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                                                                             ),
                                                                                                                                             Lit.Unit())
                                                                                                                       )
                                                                                                        ) :: Nil
                                                                                              ) :: Nil)
                                                                                 )
                                                                               ),
                                                                               Term.Name("πparSequence")
                                                                             ) :: Nil)
                                                             ) :: Nil
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                Term.Block(Term.Function(_,
                                                                         Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                 Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                 Term.ForYield(List(
                                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                                 Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                               ),
                                                                                               Lit.Unit())
                                                                         )
                                                          ) :: Nil
                                                ) :: Nil))
      ) => true
    }
  }

  /**
    * `! 13,seconds .guard(name: Int /**/).`
    *
    * @example {{{
    * for {
    *   _υ3υ <- IO {
    *     def _υ3υ(_υ2υ: `()`): String => IO[Any] = { implicit ^ =>
    *       val name: Int = ???
    *       πLs(
    *         for {
    *           _ <- IO.sleep(13L.seconds)
    *           _ <- ???.flatMap { (name, _υ4υ) =>
    *             if (_υ4υ eq null)
    *               IO.cede
    *             else
    *               for {
    *                 _υ1υ <- `π-uuid`
    *                 _    <- _υ3υ(name)(_υ1υ)
    *               } yield ()
    *           }
    *         } yield ()
    *       ).πparSequence
    *     }
    *     _υ3υ
    *   }
    *   _    <- ???.flatMap { (name, _υ4υ) =>
    *     if (_υ4υ eq null)
    *       IO.cede
    *     else
    *       for {
    *         _υ1υ <- `π-uuid`
    *         _    <- _υ3υ(name)(_υ1υ)
    *       } yield ()
    *   }
    * } yield ()
    * }}}
    */
  test("replication - input guard - typed - no parallelism & nonempty pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(-1, Some(13L->"seconds"), Some(π(λ(Symbol("guard")), λ(Symbol("name"))(using Some(\\("Int")->None)), Some(""), Some(-1), None)(id)), `+`(-1))

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Def(Nil,
                                                           Term.Name(_),
                                                           Nil,
                                                           List(Term.Param(Nil, Term.Name(_), Some(Type.Name("()")), None) :: Nil),
                                                           Some(Type.Function(Type.Name("String") :: Nil, Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil))),
                                                           Term.Block(
                                                             Term.Function(Term.Param(Mod.Implicit() :: Nil, Term.Name("^"), None, None) :: Nil,
                                                                           Term.Block(
                                                                             Defn.Val(Nil, Pat.Var(Term.Name("name")) :: Nil, Some(Type.Name("Int")), _) ::
                                                                             Term.Select(
                                                                               Term.Apply(
                                                                                 Term.Name("πLs"),
                                                                                 List(
                                                                                   Term.ForYield(List(
                                                                                                   Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("sleep")),
                                                                                                                                                   Term.Select(Lit.Long(13L), Term.Name("seconds")) :: Nil)),
                                                                                                   Enumerator.Generator(Pat.Wildcard(),
                                                                                                                        Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                                                                                                   Term.Block(Term.Function(_,
                                                                                                                                                            Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                                                                                                    Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                                                                                    Term.ForYield(List(
                                                                                                                                                                                    Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                                                                                                    Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                                                                                                                  ),
                                                                                                                                                                                  Lit.Unit())
                                                                                                                                                            )
                                                                                                                                             ) :: Nil
                                                                                                                                   ) :: Nil))
                                                                                                 ),
                                                                                                 Lit.Unit())
                                                                                 )
                                                                               ),
                                                                               Term.Name("πparSequence")
                                                                             ) :: Nil)
                                                             ) :: Nil
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                Term.Block(Term.Function(_,
                                                                         Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                 Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                 Term.ForYield(List(
                                                                                                 Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                               ),
                                                                                               Lit.Unit())
                                                                         )
                                                          ) :: Nil
                                                ) :: Nil))
      ) => true
    }
  }

  /**
    * `! 13* 13,seconds .guard(name: Int /**/).`
    *
    * @example {{{
    * for {
    *   _υ4υ <- Semaphore[IO](13)
    *   _υ3υ <- IO {
    *     def _υ3υ(_υ2υ: `()`): String => IO[Any] = { implicit ^ =>
    *       val name: Int = ???
    *       πLs(
    *         _υ4υ.release,
    *         for {
    *           _ <- IO.sleep(13L.seconds)
    *           _ <- ???.flatMap { (name, _υ5υ) =>
    *             if (_υ5υ eq null)
    *               IO.cede
    *             else
    *               for {
    *                 _    <- _υ4υ.acquire
    *                 _υ1υ <- `π-uuid`
    *                 _    <- _υ3υ(name)(_υ1υ)
    *               } yield ()
    *           }
    *         } yield ()
    *       ).πparSequence
    *     }
    *     _υ3υ
    *   }
    *   _    <- ???.flatMap { (name, _υ5υ) =>
    *     if (_υ5υ eq null)
    *       IO.cede
    *     else
    *       for {
    *         _    <- _υ4υ.acquire
    *         _υ1υ <- `π-uuid`
    *         _    <- _υ3υ(name)(_υ1υ)
    *       } yield ()
    *   }
    * } yield ()
    * }}}
    */
  test("replication - input guard - typed - parallelism >= 0 & nonempty pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(13, Some(13L->"seconds"), Some(π(λ(Symbol("guard")), λ(Symbol("name"))(using Some(\\("Int")->None)), Some(""), Some(-1), None)(id)), `+`(-1))

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Apply(Term.ApplyType(Term.Name("Semaphore"),
                                                                                      Type.Name(`\\`) :: Nil),
                                                                       Lit.Int(13) :: Nil)),
                Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Def(Nil,
                                                           Term.Name(_),
                                                           Nil,
                                                           List(Term.Param(Nil, Term.Name(_), Some(Type.Name("()")), None) :: Nil),
                                                           Some(Type.Function(Type.Name("String") :: Nil, Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil))),
                                                           Term.Block(
                                                             Term.Function(Term.Param(Mod.Implicit() :: Nil, Term.Name("^"), None, None) :: Nil,
                                                                           Term.Block(
                                                                             Defn.Val(Nil, Pat.Var(Term.Name("name")) :: Nil, Some(Type.Name("Int")), _) ::
                                                                             Term.Select(
                                                                               Term.Apply(
                                                                                 Term.Name("πLs"),
                                                                                 List(
                                                                                   Term.Select(Term.Name(_), Term.Name("release")),
                                                                                   Term.ForYield(List(
                                                                                                   Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("sleep")),
                                                                                                                                                   Term.Select(Lit.Long(13L), Term.Name("seconds")) :: Nil)),
                                                                                                   Enumerator.Generator(Pat.Wildcard(),
                                                                                                                        Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                                                                                                   Term.Block(Term.Function(_,
                                                                                                                                                            Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                                                                                                    Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                                                                                    Term.ForYield(List(
                                                                                                                                                                                    Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                                                                                                                    Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                                                                                                    Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                                                                                                                  ),
                                                                                                                                                                                  Lit.Unit())
                                                                                                                                                            )
                                                                                                                                             ) :: Nil
                                                                                                                                   ) :: Nil))
                                                                                                 ),
                                                                                                 Lit.Unit())
                                                                                 )
                                                                               ),
                                                                               Term.Name("πparSequence")
                                                                             ) :: Nil)
                                                             ) :: Nil
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                Term.Block(Term.Function(_,
                                                                         Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                 Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                 Term.ForYield(List(
                                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                                 Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), Term.Name("name") :: Nil), Term.Name(_) :: Nil))
                                                                                               ),
                                                                                               Lit.Unit())
                                                                         )
                                                          ) :: Nil
                                                ) :: Nil))
      ) => true
    }
  }

  /////////////////////////////////////////////// replication (input) (typed) //

  // REPLICATION (OUTPUT) //////////////////////////////////////////////////////

  /**
    * `! .guard<guard>.`
    *
    * @example {{{
    * for {
      _υ2υ <- IO {
        lazy val _υ2υ: String => IO[Any] = { implicit ^ =>
          πLs(
            ???.flatMap { _υ3υ =>
              if (_υ3υ eq null)
                IO.cede
              else
                for {
                  _υ1υ <- `π-uuid`
                  _    <- _υ2υ(_υ1υ)
                } yield ()
            }
          ).πparSequence
        }
        _υ2υ
      }
      _    <- ???.flatMap { _υ3υ =>
        if (_υ3υ eq null)
          IO.cede
        else
          for {
            _υ1υ <- `π-uuid`
            _    <- _υ2υ(_υ1υ)
          } yield ()
      }
    * } yield ()
    * }}}
    */
  test("replication - output guard - no parallelism & no pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(-1, None, Some(π(λ(Symbol("guard")), λ(Symbol("guard")), None, Some(-1), None)(id)), `+`(-1))

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Val(Mod.Lazy() :: Nil,
                                                           Pat.Var(Term.Name(_)) :: Nil,
                                                           Some(Type.Function(Type.Name("String") :: Nil, Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil))),
                                                           Term.Block(
                                                             Term.Function(Term.Param(Mod.Implicit() :: Nil, Term.Name("^"), None, None) :: Nil,
                                                                           Term.Select(
                                                                             Term.Apply(
                                                                               Term.Name("πLs"),
                                                                               List(
                                                                                 Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                                                            Term.Block(Term.Function(_,
                                                                                                                     Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                                                             Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                                             Term.ForYield(List(
                                                                                                                                             Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                                                             Enumerator.Generator(Pat.Wildcard(), _)
                                                                                                                                           ),
                                                                                                                                           Lit.Unit())
                                                                                                                     )
                                                                                                      ) :: Nil
                                                                                            ) :: Nil)
                                                                               )
                                                                             ),
                                                                             Term.Name("πparSequence")
                                                                           )
                                                             ) :: Nil
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                Term.Block(Term.Function(_,
                                                                         Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                 Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                 Term.ForYield(List(
                                                                                                 Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                 Enumerator.Generator(Pat.Wildcard(), _)
                                                                                               ),
                                                                                               Lit.Unit())
                                                                         )
                                                          ) :: Nil
                                                ) :: Nil)
                )
      ) => true
    }
  }

  /**
    * `! 13* .guard<guard>.`
    *
    * @example {{{
    * for {
    *   _υ3υ <- Semaphore[IO](13)
    *   _υ2υ <- IO {
    *     lazy val _υ2υ: String => IO[Any] = { implicit ^ =>
    *       πLs(
    *         _υ3υ.release,
    *         ???.flatMap { _υ4υ =>
    *           if (_υ4υ eq null)
    *             IO.cede
    *           else
    *             for {
    *               _    <- _υ3υ.acquire
    *               _υ1υ <- `π-uuid`
    *               _    <- _υ2υ(_υ1υ)
    *             } yield ()
    *         }
    *       ).πparSequence
    *     }
    *     _υ2υ
    *   }
    *   _    <- ???.flatMap { _υ4υ =>
    *     if (_υ4υ eq null)
    *       IO.cede
    *     else
    *       for {
    *         _    <- _υ3υ.acquire
    *         _υ1υ <- `π-uuid`
    *         _    <- _υ2υ(_υ1υ)
    *       } yield ()
    *   }
    * } yield ()
    * }}}
    */
  test("replication - output guard - parallelism >= 0 & no pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(13, None, Some(π(λ(Symbol("guard")), λ(Symbol("guard")), None, Some(-1), None)(id)), `+`(-1))

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Apply(Term.ApplyType(Term.Name("Semaphore"),
                                                                                      Type.Name(`\\`) :: Nil),
                                                                       Lit.Int(13) :: Nil)),
                Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Val(Mod.Lazy() :: Nil,
                                                           Pat.Var(Term.Name(_)) :: Nil,
                                                           Some(Type.Function(Type.Name("String") :: Nil, Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil))),
                                                           Term.Block(
                                                             Term.Function(Term.Param(Mod.Implicit() :: Nil, Term.Name("^"), None, None) :: Nil,
                                                                           Term.Select(
                                                                             Term.Apply(
                                                                               Term.Name("πLs"),
                                                                               List(
                                                                                 Term.Select(Term.Name(_), Term.Name("release")),
                                                                                 Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                                                            Term.Block(Term.Function(_,
                                                                                                                     Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                                                             Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                                             Term.ForYield(List(
                                                                                                                                             Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                                                                             Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                                                             Enumerator.Generator(Pat.Wildcard(), _)
                                                                                                                                           ),
                                                                                                                                           Lit.Unit())
                                                                                                                     )
                                                                                                      ) :: Nil
                                                                                            ) :: Nil)
                                                                               )
                                                                             ),
                                                                             Term.Name("πparSequence")
                                                                           )
                                                             ) :: Nil
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                Term.Block(Term.Function(_,
                                                                         Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                 Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                 Term.ForYield(List(
                                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                                 Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                 Enumerator.Generator(Pat.Wildcard(), _)
                                                                                               ),
                                                                                               Lit.Unit())
                                                                         )
                                                          ) :: Nil
                                                ) :: Nil)
                )
      ) => true
    }
  }

  /**
    * `! 13,seconds .guard<guard>.`
    *
    * @example {{{
    * for {
    *   _υ2υ <- IO {
    *     lazy val _υ2υ: String => IO[Any] = { implicit ^ =>
    *       πLs(
    *         for {
    *           _ <- IO.sleep(13L.seconds)
    *           _ <- ???.flatMap { _υ3υ =>
    *             if (_υ3υ eq null)
    *               IO.cede
    *             else
    *               for {
    *                 _υ1υ <- `π-uuid`
    *                 _    <- _υ2υ(_υ1υ)
    *               } yield ()
    *           }
    *         } yield ()
    *       ).πparSequence
    *     }
    *     _υ2υ
    *   }
    *   _    <- ???.flatMap { _υ3υ =>
    *     if (_υ3υ eq null)
    *       IO.cede
    *     else
    *       for {
    *         _υ1υ <- `π-uuid`
    *         _    <- _υ2υ(_υ1υ)
    *       } yield ()
    *   }
    * } yield ()
    * }}}
    */
  test("replication - output guard - no parallelism & nonempty pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(-1, Some(13L->"seconds"), Some(π(λ(Symbol("guard")), λ(Symbol("guard")), None, Some(-1), None)(id)), `+`(-1))

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Val(Mod.Lazy() :: Nil,
                                                           Pat.Var(Term.Name(_)) :: Nil,
                                                           Some(Type.Function(Type.Name("String") :: Nil, Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil))),
                                                           Term.Block(
                                                             Term.Function(Term.Param(Mod.Implicit() :: Nil, Term.Name("^"), None, None) :: Nil,
                                                                           Term.Select(
                                                                             Term.Apply(
                                                                               Term.Name("πLs"),
                                                                               List(
                                                                                 Term.ForYield(List(
                                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("sleep")),
                                                                                                                                                 Term.Select(Lit.Long(13L), Term.Name("seconds")) :: Nil)),
                                                                                                 Enumerator.Generator(Pat.Wildcard(),
                                                                                                                        Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                                                                                                   Term.Block(Term.Function(_,
                                                                                                                                                            Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                                                                                                    Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                                                                                    Term.ForYield(List(
                                                                                                                                                                                    Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                                                                                                    Enumerator.Generator(Pat.Wildcard(), _)
                                                                                                                                                                                  ),
                                                                                                                                                                                  Lit.Unit())
                                                                                                                                                            )
                                                                                                                                             ) :: Nil
                                                                                                                                   ) :: Nil)
                                                                                                 )
                                                                                               ),
                                                                                               Lit.Unit())
                                                                               )
                                                                             ),
                                                                             Term.Name("πparSequence")
                                                                           )
                                                             ) :: Nil
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                Term.Block(Term.Function(_,
                                                                         Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                 Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                 Term.ForYield(List(
                                                                                                 Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                 Enumerator.Generator(Pat.Wildcard(), _)
                                                                                               ),
                                                                                               Lit.Unit())
                                                                         )
                                                          ) :: Nil
                                                ) :: Nil)
                )
      ) => true
    }
  }

  /**
    * `! 13* 13,seconds .guard<guard>.`
    *
    * @example {{{
    * for {
    *   _υ3υ <- Semaphore[IO](13)
    *   _υ2υ <- IO {
    *     lazy val _υ2υ: String => IO[Any] = { implicit ^ =>
    *       πLs(
    *         _υ3υ.release,
    *         for {
    *           _ <- IO.sleep(13L.seconds)
    *           _ <- ???.flatMap { _υ4υ =>
    *             if (_υ4υ eq null)
    *               IO.cede
    *             else
    *               for {
    *                 _    <- _υ3υ.acquire
    *                 _υ1υ <- `π-uuid`
    *                 _    <- _υ2υ(_υ1υ)
    *               } yield ()
    *           }
    *         } yield ()
    *       ).πparSequence
    *     }
    *     _υ2υ
    *   }
    *   _    <- ???.flatMap { _υ4υ =>
    *     if (_υ4υ eq null)
    *       IO.cede
    *     else
    *       for {
    *         _    <- _υ3υ.acquire
    *         _υ1υ <- `π-uuid`
    *         _    <- _υ2υ(_υ1υ)
    *       } yield ()
    *   }
    * } yield ()
    * }}}
    */
  test("replication - output guard - parallelism >= 0 & nonempty pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(13, Some(13L->"seconds"), Some(π(λ(Symbol("guard")), λ(Symbol("guard")), None, Some(-1), None)(id)), `+`(-1))

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Apply(Term.ApplyType(Term.Name("Semaphore"),
                                                                                      Type.Name(`\\`) :: Nil),
                                                                       Lit.Int(13) :: Nil)),
                Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Val(Mod.Lazy() :: Nil,
                                                           Pat.Var(Term.Name(_)) :: Nil,
                                                           Some(Type.Function(Type.Name("String") :: Nil, Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil))),
                                                           Term.Block(
                                                             Term.Function(Term.Param(Mod.Implicit() :: Nil, Term.Name("^"), None, None) :: Nil,
                                                                           Term.Select(
                                                                             Term.Apply(
                                                                               Term.Name("πLs"),
                                                                               List(
                                                                                 Term.Select(Term.Name(_), Term.Name("release")),
                                                                                 Term.ForYield(List(
                                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("sleep")),
                                                                                                                                                 Term.Select(Lit.Long(13L), Term.Name("seconds")) :: Nil)),
                                                                                                 Enumerator.Generator(Pat.Wildcard(),
                                                                                                                        Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                                                                                                   Term.Block(Term.Function(_,
                                                                                                                                                            Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                                                                                                    Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                                                                                    Term.ForYield(List(
                                                                                                                                                                                    Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                                                                                                                    Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                                                                                                    Enumerator.Generator(Pat.Wildcard(), _)
                                                                                                                                                                                  ),
                                                                                                                                                                                  Lit.Unit())
                                                                                                                                                            )
                                                                                                                                             ) :: Nil
                                                                                                                                   ) :: Nil)
                                                                                                 )
                                                                                               ),
                                                                                               Lit.Unit())
                                                                               )
                                                                             ),
                                                                             Term.Name("πparSequence")
                                                                           )
                                                             ) :: Nil
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.Apply(Term.Select(_, Term.Name("flatMap")),
                                                Term.Block(Term.Function(_,
                                                                         Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Lit.Null() :: Nil),
                                                                                 Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                 Term.ForYield(List(
                                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                                 Enumerator.Generator(Pat.Var(Term.Name(_)), Term.Name("π-uuid")),
                                                                                                 Enumerator.Generator(Pat.Wildcard(), _)
                                                                                               ),
                                                                                               Lit.Unit())
                                                                         )
                                                          ) :: Nil
                                                ) :: Nil)
                )
      ) => true
    }
  }

  ////////////////////////////////////////////////////// replication (output) //

  // REPLICATION (UNGUARDED) ///////////////////////////////////////////////////

  // IMPOSSIBLE BY 'parse'

  /////////////////////////////////////////////////// replication (unguarded) //


object ProgramSuite:

  val \ = "IO"

  val `^-υidυ` = "υidυ"

  given (Enumerator.Generator, Term.Name) =
    (`* <- *`(`^-υidυ` -> Term.Name("π-uuid")), Term.Name(`^-υidυ`))
