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

package pisc
package emitter
package ce

import scala.meta.*
import dialects.Scala3

import munit.FunSuite

import parser.Calculus.*
import Meta.{ \ => _, * }
import Program.emit
import ProgramSuite.*


class ProgramSuite extends FunSuite:

  // REPLICATION (BOUND OUTPUT) ////////////////////////////////////////////////

  /**
    * `! .guard<νname1,name2>.`
    *
    * @example {{{
    * for {
    *   _υ1υ  <- IO {
    *     def _υ1υ(name1: `()`, name2: `()`): IO[Any] =
    *       πLs(
    *         for {
    *           name1 <- ν
    *           name2 <- ν
    *           _υ2υ  <- ???
    *           _     <-
    *             if (_υ2υ eq None)
    *               IO.cede
    *             else
    *               _υ1υ(name1, name2)
    *         } yield ()
    *       ).πparSequence
    *     _υ1υ
    *   }
    *   name1 <- ν
    *   name2 <- ν
    *   _υ2υ  <- ???
    *   _     <-
    *     if (_υ2υ eq None)
    *       IO.cede
    *     else
    *       _υ1υ(name1, name2)
    * } yield ()
    * }}}
    */
  test("replication - bound output guard - no parallelism & no pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(-1, None, Some(π(λ(Symbol("guard")), Some("ν"), None, λ(Symbol("name1")), λ(Symbol("name2")))), ∅())

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Def(Nil,
                                                           Term.Name(_),
                                                           Nil,
                                                           List(
                                                             Term.Param(Nil, Term.Name("name1"), Some(Type.Name("()")), None),
                                                             Term.Param(Nil, Term.Name("name2"), Some(Type.Name("()")), None),
                                                           ) :: Nil,
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.Select(
                                                             Term.Apply(
                                                               Term.Name("πLs"),
                                                               List(
                                                                 Term.ForYield(List(
                                                                                 Enumerator.Generator(Pat.Var(Term.Name("name1")), Term.Name("ν")),
                                                                                 Enumerator.Generator(Pat.Var(Term.Name("name2")), Term.Name("ν")),
                                                                                 Enumerator.Generator(Pat.Var(Term.Name(_)), _),
                                                                                 Enumerator.Generator(Pat.Wildcard(),
                                                                                                      Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Term.Name("None") :: Nil),
                                                                                                              Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                              Term.Apply(Term.Name(_), Term.Name("name1") :: Term.Name("name2") :: Nil)))
                                                                               ),
                                                                               Lit.Unit())
                                                               )
                                                             ),
                                                             Term.Name("πparSequence")
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Var(Term.Name("name1")), Term.Name("ν")),
                Enumerator.Generator(Pat.Var(Term.Name("name2")), Term.Name("ν")),
                Enumerator.Generator(Pat.Var(Term.Name(_)), _),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Term.Name("None") :: Nil),
                                             Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                             Term.Apply(Term.Name(_), Term.Name("name1") :: Term.Name("name2") :: Nil)))
      ) => true
    }
  }

  /**
    * `! 13,seconds .guard<νname1,name2>.`
    *
    * @example {{{
    * for {
    *   _υ1υ  <- IO {
    *     def _υ1υ(name1: `()`, name2: `()`): IO[Any] =
    *       πLs(
    *         for {
    *           _     <- IO.sleep(13L.seconds)
    *           name1 <- ν
    *           name2 <- ν
    *           _υ2υ  <- ???
    *           _     <-
    *             if (_υ2υ eq None)
    *               IO.cede
    *             else
    *               _υ1υ(name1, name2)
    *         } yield ()
    *       ).πparSequence
    *     _υ1υ
    *   }
    *   name1 <- ν
    *   name2 <- ν
    *   _υ2υ  <- ???
    *   _     <-
    *     if (_υ2υ eq None)
    *       IO.cede
    *     else
    *       _υ1υ(name1, name2)
    * } yield ()
    * }}}
    */
  test("replication - bound output guard - no parallelism & nonempty pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(-1, Some(13L->"seconds"), Some(π(λ(Symbol("guard")), Some("ν"), None, λ(Symbol("name1")), λ(Symbol("name2")))), ∅())

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Def(Nil,
                                                           Term.Name(_),
                                                           Nil,
                                                           List(
                                                             Term.Param(Nil, Term.Name("name1"), Some(Type.Name("()")), None),
                                                             Term.Param(Nil, Term.Name("name2"), Some(Type.Name("()")), None),
                                                           ) :: Nil,
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.Select(
                                                             Term.Apply(
                                                               Term.Name("πLs"),
                                                               List(
                                                                 Term.ForYield(List(
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("sleep")),
                                                                                                                                 Term.Select(Lit.Long(13L), Term.Name("seconds")) :: Nil)),
                                                                                 Enumerator.Generator(Pat.Var(Term.Name("name1")), Term.Name("ν")),
                                                                                 Enumerator.Generator(Pat.Var(Term.Name("name2")), Term.Name("ν")),
                                                                                 Enumerator.Generator(Pat.Var(Term.Name(_)), _),
                                                                                 Enumerator.Generator(Pat.Wildcard(),
                                                                                                      Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Term.Name("None") :: Nil),
                                                                                                              Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                              Term.Apply(Term.Name(_), Term.Name("name1") :: Term.Name("name2") :: Nil)))
                                                                               ),
                                                                               Lit.Unit())
                                                               )
                                                             ),
                                                             Term.Name("πparSequence")
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Var(Term.Name("name1")), Term.Name("ν")),
                Enumerator.Generator(Pat.Var(Term.Name("name2")), Term.Name("ν")),
                Enumerator.Generator(Pat.Var(Term.Name(_)), _),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Term.Name("None") :: Nil),
                                             Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                             Term.Apply(Term.Name(_), Term.Name("name1") :: Term.Name("name2") :: Nil)))
      ) => true
    }
  }

  /**
    * `! 13* .guard<νname1,name2>.`
    *
    * @example {{{
    * for {
    *   _υ3υ  <- Semaphore[IO](13)
    *   _υ1υ  <- IO {
    *     def _υ1υ(name1: `()`, name2: `()`): IO[Any] =
    *       πLs(
    *         _υ3υ.release,
    *         for {
    *           _     <- _υ3υ.acquire
    *           name1 <- ν
    *           name2 <- ν
    *           _υ2υ  <- ???
    *           _     <-
    *             if (_υ2υ eq None)
    *               IO.cede
    *             else
    *               _υ1υ(name1, name2)
    *         } yield ()
    *       ).πparSequence
    *     _υ1υ
    *   }
    *   _     <- _υ3υ.acquire
    *   name1 <- ν
    *   name2 <- ν
    *   _υ2υ  <- ???
    *   _     <-
    *     if (_υ2υ eq None)
    *       IO.cede
    *     else
    *       _υ1υ(name1, name2)
    * } yield ()
    * }}}
    */
  test("replication - bound output guard - parallelism >= 0 & no pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(13, None, Some(π(λ(Symbol("guard")), Some("ν"), None, λ(Symbol("name1")), λ(Symbol("name2")))), ∅())

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
                                                           List(
                                                             Term.Param(Nil, Term.Name("name1"), Some(Type.Name("()")), None),
                                                             Term.Param(Nil, Term.Name("name2"), Some(Type.Name("()")), None),
                                                           ) :: Nil,
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.Select(
                                                             Term.Apply(
                                                               Term.Name("πLs"),
                                                               List(
                                                                 Term.Select(Term.Name(_), Term.Name("release")),
                                                                 Term.ForYield(List(
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                 Enumerator.Generator(Pat.Var(Term.Name("name1")), Term.Name("ν")),
                                                                                 Enumerator.Generator(Pat.Var(Term.Name("name2")), Term.Name("ν")),
                                                                                 Enumerator.Generator(Pat.Var(Term.Name(_)), _),
                                                                                 Enumerator.Generator(Pat.Wildcard(),
                                                                                                      Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Term.Name("None") :: Nil),
                                                                                                              Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                              Term.Apply(Term.Name(_), Term.Name("name1") :: Term.Name("name2") :: Nil)))
                                                                               ),
                                                                               Lit.Unit())
                                                               )
                                                             ),
                                                             Term.Name("πparSequence")
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                Enumerator.Generator(Pat.Var(Term.Name("name1")), Term.Name("ν")),
                Enumerator.Generator(Pat.Var(Term.Name("name2")), Term.Name("ν")),
                Enumerator.Generator(Pat.Var(Term.Name(_)), _),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Term.Name("None") :: Nil),
                                             Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                             Term.Apply(Term.Name(_), Term.Name("name1") :: Term.Name("name2") :: Nil)))
      ) => true
    }
  }

  /**
    * `! 13* 13,seconds .guard<νname1,name2>.`
    *
    * @example {{{
    * for {
    *   _υ3υ  <- Semaphore[IO](13)
    *   _υ1υ  <- IO {
    *     def _υ1υ(name1: `()`, name2: `()`): IO[Any] =
    *       πLs(
    *         _υ3υ.release,
    *         for {
    *           _     <- IO.sleep(13L.seconds)
    *           _     <- _υ3υ.acquire
    *           name1 <- ν
    *           name2 <- ν
    *           _υ2υ  <- ???
    *           _     <-
    *             if (_υ2υ eq None)
    *               IO.cede
    *             else
    *               _υ1υ(name1, name2)
    *         } yield ()
    *       ).πparSequence
    *     _υ1υ
    *   }
    *   _     <- _υ3υ.acquire
    *   name1 <- ν
    *   name2 <- ν
    *   _υ2υ  <- ???
    *   _     <-
    *     if (_υ2υ eq None)
    *       IO.cede
    *     else
    *       _υ1υ(name1, name2)
    * } yield ()
    * }}}
    */
  test("replication - bound output guard - parallelism >= 0 & nonempty pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(13, Some(13L->"seconds"), Some(π(λ(Symbol("guard")), Some("ν"), None, λ(Symbol("name1")), λ(Symbol("name2")))), ∅())

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
                                                           List(
                                                             Term.Param(Nil, Term.Name("name1"), Some(Type.Name("()")), None),
                                                             Term.Param(Nil, Term.Name("name2"), Some(Type.Name("()")), None),
                                                           ) :: Nil,
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.Select(
                                                             Term.Apply(
                                                               Term.Name("πLs"),
                                                               List(
                                                                 Term.Select(Term.Name(_), Term.Name("release")),
                                                                 Term.ForYield(List(
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("sleep")),
                                                                                                                                 Term.Select(Lit.Long(13L), Term.Name("seconds")) :: Nil)),
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                 Enumerator.Generator(Pat.Var(Term.Name("name1")), Term.Name("ν")),
                                                                                 Enumerator.Generator(Pat.Var(Term.Name("name2")), Term.Name("ν")),
                                                                                 Enumerator.Generator(Pat.Var(Term.Name(_)), _),
                                                                                 Enumerator.Generator(Pat.Wildcard(),
                                                                                                      Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Term.Name("None") :: Nil),
                                                                                                              Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                              Term.Apply(Term.Name(_), Term.Name("name1") :: Term.Name("name2") :: Nil)))
                                                                               ),
                                                                               Lit.Unit())
                                                               )
                                                             ),
                                                             Term.Name("πparSequence")
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                Enumerator.Generator(Pat.Var(Term.Name("name1")), Term.Name("ν")),
                Enumerator.Generator(Pat.Var(Term.Name("name2")), Term.Name("ν")),
                Enumerator.Generator(Pat.Var(Term.Name(_)), _),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Term.Name("None") :: Nil),
                                             Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                             Term.Apply(Term.Name(_), Term.Name("name1") :: Term.Name("name2") :: Nil)))
      ) => true
    }
  }

  //////////////////////////////////////////////// replication (bound output) //

  // REPLICATION (INPUT) ///////////////////////////////////////////////////////

  /**
    * `! .guard(name1,name2).`
    *
    * @example {{{
    * for {
    *   _υ1υ              <- IO {
    *     def _υ1υ(name1: `()`, name2: `()`): IO[Any] =
    *       if (!name1)
    *         IO.cede
    *       else {
    *         πLs(
    *           for {
    *             Seq(name1, name2) <- ???
    *             _                 <- _υ1υ(name1, name2)
    *           } yield ()
    *         ).πparSequence
    *       }
    *     _υ1υ
    *   }
    *   Seq(name1, name2) <- ???
    *   _                 <- _υ1υ(name1, name2)
    * } yield ()
    * }}}
    */
  test("replication - input guard - no parallelism & no pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(-1, None, Some(π(λ(Symbol("guard")), Some(""), None, λ(Symbol("name1")), λ(Symbol("name2")))), ∅())

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Def(Nil,
                                                           Term.Name(_),
                                                           Nil,
                                                           List(
                                                             Term.Param(Nil, Term.Name("name1"), Some(Type.Name("()")), None),
                                                             Term.Param(Nil, Term.Name("name2"), Some(Type.Name("()")), None),
                                                           ) :: Nil,
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.If(Term.ApplyUnary(Term.Name("!"), Term.Name("name1")),
                                                                   Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                   Term.Block(
                                                                     Term.Select(
                                                                       Term.Apply(
                                                                         Term.Name("πLs"),
                                                                         List(
                                                                           Term.ForYield(List(
                                                                                           Enumerator.Generator(Pat.Extract(Term.Name("Seq"), Pat.Var(Term.Name("name1")) :: Pat.Var(Term.Name("name2")) :: Nil), _),
                                                                                           Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name1") :: Term.Name("name2") :: Nil))
                                                                                         ),
                                                                                         Lit.Unit())
                                                                         )
                                                                       ),
                                                                       Term.Name("πparSequence")
                                                                     ) :: Nil
                                                                   )
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Extract(Term.Name("Seq"), Pat.Var(Term.Name("name1")) :: Pat.Var(Term.Name("name2")) :: Nil), _),
                Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name1") :: Term.Name("name2") :: Nil))
      ) => true
    }
  }

  /**
    * `! 13* .guard(name1,name2).`
    *
    * @example {{{
    * for {
    *   _υ2υ              <- Semaphore[IO](13)
    *   _υ1υ              <- IO {
    *     def _υ1υ(name1: `()`, name2: `()`): IO[Any] =
    *       if (!name1)
    *         _υ2υ.release
    *       else {
    *         πLs(
    *           _υ2υ.release,
    *           for {
    *             _                 <- _υ2υ.acquire
    *             Seq(name1, name2) <- ???
    *             _                 <- _υ1υ(name1, name2)
    *           } yield ()
    *         ).πparSequence
    *       }
    *     _υ1υ
    *   }
    *   _                 <- _υ2υ.acquire
    *   Seq(name1, name2) <- ???
    *   _                 <- _υ1υ(name1, name2)
    * } yield ()
    * }}}
    */
  test("replication - input guard - parallelism >= 0 & no pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(13, None, Some(π(λ(Symbol("guard")), Some(""), None, λ(Symbol("name1")), λ(Symbol("name2")))), ∅())

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
                                                           List(
                                                             Term.Param(Nil, Term.Name("name1"), Some(Type.Name("()")), None),
                                                             Term.Param(Nil, Term.Name("name2"), Some(Type.Name("()")), None),
                                                           ) :: Nil,
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.If(Term.ApplyUnary(Term.Name("!"), Term.Name("name1")),
                                                                   Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                   Term.Block(
                                                                     Term.Select(
                                                                       Term.Apply(
                                                                         Term.Name("πLs"),
                                                                         List(
                                                                           Term.Select(Term.Name(_), Term.Name("release")),
                                                                           Term.ForYield(List(
                                                                                           Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                           Enumerator.Generator(Pat.Extract(Term.Name("Seq"), Pat.Var(Term.Name("name1")) :: Pat.Var(Term.Name("name2")) :: Nil), _),
                                                                                           Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name1") :: Term.Name("name2") :: Nil))
                                                                                         ),
                                                                                         Lit.Unit())
                                                                         )
                                                                       ),
                                                                       Term.Name("πparSequence")
                                                                     ) :: Nil
                                                                   )
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                Enumerator.Generator(Pat.Extract(Term.Name("Seq"), Pat.Var(Term.Name("name1")) :: Pat.Var(Term.Name("name2")) :: Nil), _),
                Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name1") :: Term.Name("name2") :: Nil))
      ) => true
    }
  }

  /**
    * `! 13,seconds .guard(name1,name2).`
    *
    * @example {{{
    * for {
    *   _υ1υ              <- IO {
    *     def _υ1υ(name1: `()`, name2: `()`): IO[Any] =
    *       if (!name1)
    *         IO.cede
    *       else {
    *         πLs(
    *           for {
    *             _                 <- IO.sleep(13L.seconds)
    *             Seq(name1, name2) <- ???
    *             _                 <- _υ1υ(name1, name2)
    *           } yield ()
    *         ).πparSequence
    *       }
    *     _υ1υ
    *   }
    *   Seq(name1, name2) <- ???
    *   _                 <- _υ1υ(name1, name2)
    * } yield ()
    * }}}
    */
  test("replication - input guard - no parallelism & nonempty pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(-1, Some(13L->"seconds"), Some(π(λ(Symbol("guard")), Some(""), None, λ(Symbol("name1")), λ(Symbol("name2")))), ∅())

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Def(Nil,
                                                           Term.Name(_),
                                                           Nil,
                                                           List(
                                                             Term.Param(Nil, Term.Name("name1"), Some(Type.Name("()")), None),
                                                             Term.Param(Nil, Term.Name("name2"), Some(Type.Name("()")), None),
                                                           ) :: Nil,
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.If(Term.ApplyUnary(Term.Name("!"), Term.Name("name1")),
                                                                   Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                   Term.Block(
                                                                     Term.Select(
                                                                       Term.Apply(
                                                                         Term.Name("πLs"),
                                                                         List(
                                                                           Term.ForYield(List(
                                                                                           Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("sleep")),
                                                                                                                                           Term.Select(Lit.Long(13L), Term.Name("seconds")) :: Nil)),
                                                                                           Enumerator.Generator(Pat.Extract(Term.Name("Seq"), Pat.Var(Term.Name("name1")) :: Pat.Var(Term.Name("name2")) :: Nil), _),
                                                                                           Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name1") :: Term.Name("name2") :: Nil))
                                                                                         ),
                                                                                         Lit.Unit())
                                                                         )
                                                                       ),
                                                                       Term.Name("πparSequence")
                                                                     ) :: Nil
                                                                   )
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Extract(Term.Name("Seq"), Pat.Var(Term.Name("name1")) :: Pat.Var(Term.Name("name2")) :: Nil), _),
                Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name1") :: Term.Name("name2") :: Nil))
      ) => true
    }
  }

  /**
    * `! 13* 13,seconds .guard(name1,name2).`
    *
    * @example {{{
    * for {
    *   _υ2υ              <- Semaphore[IO](13)
    *   _υ1υ              <- IO {
    *     def _υ1υ(name1: `()`, name2: `()`): IO[Any] =
    *       if (!name1)
    *         _υ2υ.release
    *       else {
    *         πLs(
    *           _υ2υ.release,
    *           for {
    *             _                 <- IO.sleep(13L.seconds)
    *             _                 <- _υ2υ.acquire
    *             Seq(name1, name2) <- ???
    *             _                 <- _υ1υ(name1, name2)
    *           } yield ()
    *         ).πparSequence
    *       }
    *     _υ1υ
    *   }
    *   _                 <- _υ2υ.acquire
    *   Seq(name1, name2) <- ???
    *   _                 <- _υ1υ(name1, name2)
    * } yield ()
    * }}}
    */
  test("replication - input guard - parallelism >= 0 & nonempty pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(13, Some(13L->"seconds"), Some(π(λ(Symbol("guard")), Some(""), None, λ(Symbol("name1")), λ(Symbol("name2")))), ∅())

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
                                                           List(
                                                             Term.Param(Nil, Term.Name("name1"), Some(Type.Name("()")), None),
                                                             Term.Param(Nil, Term.Name("name2"), Some(Type.Name("()")), None),
                                                           ) :: Nil,
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.If(Term.ApplyUnary(Term.Name("!"), Term.Name("name1")),
                                                                   Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                   Term.Block(
                                                                     Term.Select(
                                                                       Term.Apply(
                                                                         Term.Name("πLs"),
                                                                         List(
                                                                           Term.Select(Term.Name(_), Term.Name("release")),
                                                                           Term.ForYield(List(
                                                                                           Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("sleep")),
                                                                                                                                           Term.Select(Lit.Long(13L), Term.Name("seconds")) :: Nil)),
                                                                                           Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                           Enumerator.Generator(Pat.Extract(Term.Name("Seq"), Pat.Var(Term.Name("name1")) :: Pat.Var(Term.Name("name2")) :: Nil), _),
                                                                                           Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name1") :: Term.Name("name2") :: Nil))
                                                                                         ),
                                                                                         Lit.Unit())
                                                                         )
                                                                       ),
                                                                       Term.Name("πparSequence")
                                                                     ) :: Nil
                                                                   )
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                Enumerator.Generator(Pat.Extract(Term.Name("Seq"), Pat.Var(Term.Name("name1")) :: Pat.Var(Term.Name("name2")) :: Nil), _),
                Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name1") :: Term.Name("name2") :: Nil))
      ) => true
    }
  }

  /////////////////////////////////////////////////////// replication (input) //

  // REPLICATION (INPUT) (TYPED) ///////////////////////////////////////////////

  /**
    * `! .guard(name: Int /**/ ).`
    *
    * @example {{{
    * for {
    *   _υ2υ      <- IO {
    *     def _υ2υ(_υ1υ: `()`): IO[Any] =
    *       if (!_υ1υ)
    *         IO.cede
    *       else {
    *         val name: Int = ???
    *         πLs(
    *           for {
    *             Seq(name) <- ???
    *             _         <- _υ2υ(name)
    *           } yield ()
    *         ).πparSequence
    *       }
    *     _υ2υ
    *   }
    *   Seq(name) <- ???
    *   _         <- _υ2υ(name)
    * } yield ()
    * }}}
    */
  test("replication - input guard - typed - no parallelism & no pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(-1, None, Some(π(λ(Symbol("guard")), Some(""), None, λ(Symbol("name"))(using Some(\\("Int")->None)))), ∅())

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Def(Nil,
                                                           Term.Name(_),
                                                           Nil,
                                                           List(Term.Param(Nil, Term.Name(_), Some(Type.Name("()")), None) :: Nil),
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.If(Term.ApplyUnary(Term.Name("!"), Term.Name(_)),
                                                                   Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                   Term.Block(List(
                                                                                Defn.Val(Nil, Pat.Var(Term.Name("name")) :: Nil, Some(Type.Name("Int")), _),
                                                                                Term.Select(
                                                                                  Term.Apply(
                                                                                    Term.Name("πLs"),
                                                                                    List(
                                                                                      Term.ForYield(List(
                                                                                                      Enumerator.Generator(Pat.Extract(Term.Name("Seq"), Pat.Var(Term.Name("name")) :: Nil), _),
                                                                                                      Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name") :: Nil))
                                                                                                    ),
                                                                                                    Lit.Unit())
                                                                                    )
                                                                                  ),
                                                                                  Term.Name("πparSequence")
                                                                                )
                                                                              )
                                                                   )
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Extract(Term.Name("Seq"), Pat.Var(Term.Name("name")) :: Nil), _),
                Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name") :: Nil))
      ) => true
    }
  }

  /**
    * `! 13* .guard(name: Int /**/ ).`
    *
    * @example {{{
    * for {
    *   _υ3υ      <- Semaphore[IO](13)
    *   _υ2υ      <- IO {
    *     def _υ2υ(_υ1υ: `()`): IO[Any] =
    *       if (!_υ1υ)
    *         _υ3υ.release
    *       else {
    *         val name: Int = ???
    *         πLs(
    *           _υ3υ.release,
    *           for {
    *             _         <- _υ3υ.acquire
    *             Seq(name) <- ???
    *             _         <- _υ2υ(name)
    *           } yield ()
    *         ).πparSequence
    *       }
    *     _υ2υ
    *   }
    *   _         <- _υ3υ.acquire
    *   Seq(name) <- ???
    *   _         <- _υ2υ(name)
    * } yield ()
    * }}}
    */
  test("replication - input guard - typed - parallelism >= 0 & no pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(13, None, Some(π(λ(Symbol("guard")), Some(""), None, λ(Symbol("name"))(using Some(\\("Int")->None)))), ∅())

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
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.If(Term.ApplyUnary(Term.Name("!"), Term.Name(_)),
                                                                   Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                   Term.Block(List(
                                                                                Defn.Val(Nil, Pat.Var(Term.Name("name")) :: Nil, Some(Type.Name("Int")), _),
                                                                                Term.Select(
                                                                                  Term.Apply(
                                                                                    Term.Name("πLs"),
                                                                                    List(
                                                                                      Term.Select(Term.Name(_), Term.Name("release")),
                                                                                      Term.ForYield(List(
                                                                                                      Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                                      Enumerator.Generator(Pat.Extract(Term.Name("Seq"), Pat.Var(Term.Name("name")) :: Nil), _),
                                                                                                      Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name") :: Nil))
                                                                                                    ),
                                                                                                    Lit.Unit())
                                                                                    )
                                                                                  ),
                                                                                  Term.Name("πparSequence")
                                                                                )
                                                                              )
                                                                   )
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                Enumerator.Generator(Pat.Extract(Term.Name("Seq"), Pat.Var(Term.Name("name")) :: Nil), _),
                Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name") :: Nil))
      ) => true
    }
  }

  /**
    * `! 13,seconds .guard(name: Int /**/ ).`
    *
    * @example {{{
    * for {
    *   _υ2υ      <- IO {
    *     def _υ2υ(_υ1υ: `()`): IO[Any] =
    *       if (!_υ1υ)
    *         IO.cede
    *       else {
    *         val name: Int = ???
    *         πLs(
    *           for {
    *             _         <- IO.sleep(13L.seconds)
    *             Seq(name) <- ???
    *             _         <- _υ2υ(name)
    *           } yield ()
    *         ).πparSequence
    *       }
    *     _υ2υ
    *   }
    *   Seq(name) <- ???
    *   _         <- _υ2υ(name)
    * } yield ()
    * }}}
    */
  test("replication - input guard - typed - no parallelism & nonempty pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(-1, Some(13L->"seconds"), Some(π(λ(Symbol("guard")), Some(""), None, λ(Symbol("name"))(using Some(\\("Int")->None)))), ∅())

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Def(Nil,
                                                           Term.Name(_),
                                                           Nil,
                                                           List(Term.Param(Nil, Term.Name(_), Some(Type.Name("()")), None) :: Nil),
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.If(Term.ApplyUnary(Term.Name("!"), Term.Name(_)),
                                                                   Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                   Term.Block(List(
                                                                                Defn.Val(Nil, Pat.Var(Term.Name("name")) :: Nil, Some(Type.Name("Int")), _),
                                                                                Term.Select(
                                                                                  Term.Apply(
                                                                                    Term.Name("πLs"),
                                                                                    List(
                                                                                      Term.ForYield(List(
                                                                                                      Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("sleep")),
                                                                                                                                                      Term.Select(Lit.Long(13L), Term.Name("seconds")) :: Nil)),
                                                                                                      Enumerator.Generator(Pat.Extract(Term.Name("Seq"), Pat.Var(Term.Name("name")) :: Nil), _),
                                                                                                      Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name") :: Nil))
                                                                                                    ),
                                                                                                    Lit.Unit())
                                                                                    )
                                                                                  ),
                                                                                  Term.Name("πparSequence")
                                                                                )
                                                                              )
                                                                   )
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Extract(Term.Name("Seq"), Pat.Var(Term.Name("name")) :: Nil), _),
                Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name") :: Nil))
      ) => true
    }
  }

  /**
    * `! 13* 13,seconds .guard(name: Int /**/ ).`
    *
    * @example {{{
    * for {
    *   _υ3υ      <- Semaphore[IO](13)
    *   _υ2υ      <- IO {
    *     def _υ2υ(_υ1υ: `()`): IO[Any] =
    *       if (!_υ1υ)
    *         _υ3υ.release
    *       else {
    *         val name: Int = ???
    *         πLs(
    *           _υ3υ.release,
    *           for {
    *             _         <- IO.sleep(13L.seconds)
    *             _         <- _υ3υ.acquire
    *             Seq(name) <- ???
    *             _         <- _υ2υ(name)
    *           } yield ()
    *         ).πparSequence
    *       }
    *     _υ2υ
    *   }
    *   _         <- _υ3υ.acquire
    *   Seq(name) <- ???
    *   _         <- _υ2υ(name)
    * } yield ()
    * }}}
    */
  test("replication - input guard - typed - parallelism >= 0 & nonempty pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(13, Some(13L->"seconds"), Some(π(λ(Symbol("guard")), Some(""), None, λ(Symbol("name"))(using Some(\\("Int")->None)))), ∅())

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
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.If(Term.ApplyUnary(Term.Name("!"), Term.Name(_)),
                                                                   Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                   Term.Block(List(
                                                                                Defn.Val(Nil, Pat.Var(Term.Name("name")) :: Nil, Some(Type.Name("Int")), _),
                                                                                Term.Select(
                                                                                  Term.Apply(
                                                                                    Term.Name("πLs"),
                                                                                    List(
                                                                                      Term.Select(Term.Name(_), Term.Name("release")),
                                                                                      Term.ForYield(List(
                                                                                                      Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("sleep")),
                                                                                                                                                      Term.Select(Lit.Long(13L), Term.Name("seconds")) :: Nil)),
                                                                                                      Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                                      Enumerator.Generator(Pat.Extract(Term.Name("Seq"), Pat.Var(Term.Name("name")) :: Nil), _),
                                                                                                      Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name") :: Nil))
                                                                                                    ),
                                                                                                    Lit.Unit())
                                                                                    )
                                                                                  ),
                                                                                  Term.Name("πparSequence")
                                                                                )
                                                                              )
                                                                   )
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                Enumerator.Generator(Pat.Extract(Term.Name("Seq"), Pat.Var(Term.Name("name")) :: Nil), _),
                Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name") :: Nil))
      ) => true
    }
  }

  /////////////////////////////////////////////// replication (input) (typed) //

  // REPLICATION (OUTPUT) //////////////////////////////////////////////////////

  /**
    * `! .guard<guard,guard>.`
    *
    * @example {{{
    * for {
    *   _υ1υ <- IO {
    *     lazy val _υ1υ: IO[Any] =
    *       πLs(
    *         for {
    *           _υ2υ <- ???
    *           _    <-
    *             if (_υ2υ eq None)
    *               IO.cede
    *             else
    *               _υ1υ
    *         } yield ()
    *       ).πparSequence
    *     _υ1υ
    *   }
    *   _υ2υ <- ???
    *   _    <-
    *     if (_υ2υ eq None)
    *       IO.cede
    *     else
    *       _υ1υ
    * } yield ()
    * }}}
    */
  test("replication - output guard - no parallelism & no pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(-1, None, Some(π(λ(Symbol("guard")), None, None, λ(Symbol("guard")), λ(Symbol("guard")))), ∅())

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Val(Mod.Lazy() :: Nil,
                                                           Pat.Var(Term.Name(_)) :: Nil,
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.Select(
                                                             Term.Apply(
                                                               Term.Name("πLs"),
                                                               List(
                                                                 Term.ForYield(List(
                                                                                 Enumerator.Generator(Pat.Var(Term.Name(_)), _),
                                                                                 Enumerator.Generator(Pat.Wildcard(),
                                                                                                      Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Term.Name("None") :: Nil),
                                                                                                              Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                              Term.Name(_)))
                                                                               ),
                                                                               Lit.Unit())
                                                               )
                                                             ),
                                                             Term.Name("πparSequence")
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Var(Term.Name(_)), _),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Term.Name("None") :: Nil),
                                             Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                             Term.Name(_)))
      ) => true
    }
  }

  /**
    * `! 13* .guard<guard,guard>.`
    *
    * @example {{{
    * for {
    *   _υ3υ <- Semaphore[IO](13)
    *   _υ1υ <- IO {
    *     lazy val _υ1υ: IO[Any] =
    *       πLs(
    *         _υ3υ.release,
    *         for {
    *           _    <- _υ3υ.acquire
    *           _υ2υ <- ???
    *           _    <-
    *             if (_υ2υ eq None)
    *               IO.cede
    *             else
    *               _υ1υ
    *         } yield ()
    *       ).πparSequence
    *     _υ1υ
    *   }
    *   _    <- _υ3υ.acquire
    *   _υ2υ <- ???
    *   _    <-
    *     if (_υ2υ eq None)
    *       IO.cede
    *     else
    *       _υ1υ
    * } yield ()
    * }}}
    */
  test("replication - output guard - parallelism >= 0 & no pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(13, None, Some(π(λ(Symbol("guard")), None, None, λ(Symbol("guard")), λ(Symbol("guard")))), ∅())

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
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.Select(
                                                             Term.Apply(
                                                               Term.Name("πLs"),
                                                               List(
                                                                 Term.Select(Term.Name(_), Term.Name("release")),
                                                                 Term.ForYield(List(
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                 Enumerator.Generator(Pat.Var(Term.Name(_)), _),
                                                                                 Enumerator.Generator(Pat.Wildcard(),
                                                                                                      Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Term.Name("None") :: Nil),
                                                                                                              Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                              Term.Name(_)))
                                                                               ),
                                                                               Lit.Unit())
                                                               )
                                                             ),
                                                             Term.Name("πparSequence")
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                Enumerator.Generator(Pat.Var(Term.Name(_)), _),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Term.Name("None") :: Nil),
                                             Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                             Term.Name(_)))
      ) => true
    }
  }

  /**
    * `! 13,seconds .guard<guard,guard>.`
    *
    * @example {{{
    * for {
    *   _υ1υ <- IO {
    *     lazy val _υ1υ: IO[Any] =
    *       πLs(
    *         for {
    *           _    <- IO.sleep(13L.seconds)
    *           _υ2υ <- ???
    *           _    <-
    *             if (_υ2υ eq None)
    *               IO.cede
    *             else
    *               _υ1υ
    *         } yield ()
    *       ).πparSequence
    *     _υ1υ
    *   }
    *   _υ2υ <- ???
    *   _    <-
    *     if (_υ2υ eq None)
    *       IO.cede
    *     else
    *       _υ1υ
    * } yield ()
    * }}}
    */
  test("replication - output guard - no parallelism & nonempty pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(-1, Some(13L->"seconds"), Some(π(λ(Symbol("guard")), None, None, λ(Symbol("guard")), λ(Symbol("guard")))), ∅())

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Val(Mod.Lazy() :: Nil,
                                                           Pat.Var(Term.Name(_)) :: Nil,
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.Select(
                                                             Term.Apply(
                                                               Term.Name("πLs"),
                                                               List(
                                                                 Term.ForYield(List(
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("sleep")),
                                                                                                                                 Term.Select(Lit.Long(13L), Term.Name("seconds")) :: Nil)),
                                                                                 Enumerator.Generator(Pat.Var(Term.Name(_)), _),
                                                                                 Enumerator.Generator(Pat.Wildcard(),
                                                                                                      Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Term.Name("None") :: Nil),
                                                                                                              Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                              Term.Name(_)))
                                                                               ),
                                                                               Lit.Unit())
                                                               )
                                                             ),
                                                             Term.Name("πparSequence")
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Var(Term.Name(_)), _),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Term.Name("None") :: Nil),
                                             Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                             Term.Name(_)))
      ) => true
    }
  }

  /**
    * `! 13* 13,seconds .guard<guard,guard>.`
    *
    * @example {{{
    * for {
    *   _υ3υ <- Semaphore[IO](13)
    *   _υ1υ <- IO {
    *     lazy val _υ1υ: IO[Any] =
    *       πLs(
    *         _υ3υ.release,
    *         for {
    *           _    <- IO.sleep(13L.seconds)
    *           _υ2υ <- ???
    *           _    <-
    *             if (_υ2υ eq None)
    *               IO.cede
    *             else
    *               for {
    *                 _ <- _υ3υ.acquire
    *                 _ <- _υ1υ
    *               } yield ()
    *         } yield ()
    *       ).πparSequence
    *     _υ1υ
    *   }
    *   _υ2υ <- ???
    *   _    <-
    *     if (_υ2υ eq None)
    *       IO.cede
    *     else
    *       for {
    *         _ <- _υ3υ.acquire
    *         _ <- _υ1υ
    *       } yield ()
    * } yield ()
    * }}}
    */
  test("replication - output guard - parallelism >= 0 & nonempty pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(13, Some(13L->"seconds"), Some(π(λ(Symbol("guard")), None, None, λ(Symbol("guard")), λ(Symbol("guard")))), ∅())

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
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.Select(
                                                             Term.Apply(
                                                               Term.Name("πLs"),
                                                               List(
                                                                 Term.Select(Term.Name(_), Term.Name("release")),
                                                                 Term.ForYield(List(
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("sleep")),
                                                                                                                                 Term.Select(Lit.Long(13L), Term.Name("seconds")) :: Nil)),
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                 Enumerator.Generator(Pat.Var(Term.Name(_)), _),
                                                                                 Enumerator.Generator(Pat.Wildcard(),
                                                                                                      Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Term.Name("None") :: Nil),
                                                                                                              Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                                                              Term.Name(_)))
                                                                               ),
                                                                               Lit.Unit())
                                                               )
                                                             ),
                                                             Term.Name("πparSequence")
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                Enumerator.Generator(Pat.Var(Term.Name(_)), _),
                Enumerator.Generator(Pat.Wildcard(),
                                     Term.If(Term.ApplyInfix(Term.Name(_), Term.Name("eq"), Nil, Term.Name("None") :: Nil),
                                             Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                             Term.Name(_)))
      ) => true
    }
  }

  ////////////////////////////////////////////////////// replication (output) //

  // REPLICATION (UNGUARDED) ///////////////////////////////////////////////////

  /**
    * `!`
    *
    * @example {{{
    * for {
    *   _υ1υ <- IO {
    *     lazy val _υ1υ: IO[Any] =
    *       πLs(
    *         for {
    *           _ <- IO.unit
    *           _ <- _υ1υ
    *         } yield ()
    *       ).πparSequence
    *     _υ1υ
    *   }
    *   _    <- _υ1υ
    * } yield ()
    * }}}
    */
  test("replication - output - no parallelism & no pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(-1, None, None, ∅())

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Val(Mod.Lazy() :: Nil,
                                                           Pat.Var(Term.Name(_)) :: Nil,
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.Select(
                                                             Term.Apply(
                                                               Term.Name("πLs"),
                                                               List(
                                                                 Term.ForYield(List(
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(`\\`), Term.Name("unit"))),
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Name(_))
                                                                               ),
                                                                               Lit.Unit())
                                                               )
                                                             ),
                                                             Term.Name("πparSequence")
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(), Term.Name(_))
      ) => true
    }
  }

  /**
    * `! 13*`
    *
    * @example {{{
    * for {
    *   _υ2υ <- Semaphore[IO](13)
    *   _υ1υ <- IO {
    *     lazy val _υ1υ: IO[Any] =
    *       πLs(
    *         _υ2υ.release,
    *         for {
    *           _ <- IO.unit
    *           _ <- _υ2υ.acquire
    *           _ <- _υ1υ
    *         } yield ()
    *       ).πparSequence
    *     _υ1υ
    *   }
    *   _    <- _υ2υ.acquire
    *   _    <- _υ1υ
    * } yield ()
    * }}}
    */
  test("replication - output - parallelism >= 0 & no pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(13, None, None, ∅())

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
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.Select(
                                                             Term.Apply(
                                                               Term.Name("πLs"),
                                                               List(
                                                                 Term.Select(Term.Name(_), Term.Name("release")),
                                                                 Term.ForYield(List(
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(`\\`), Term.Name("unit"))),
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Name(_))
                                                                               ),
                                                                               Lit.Unit())
                                                               )
                                                             ),
                                                             Term.Name("πparSequence")
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                Enumerator.Generator(Pat.Wildcard(), Term.Name(_))
      ) => true
    }
  }

  /**
    * `! 13,seconds`
    *
    * @example {{{
    * for {
    *   _υ1υ <- IO {
    *     lazy val _υ1υ: IO[Any] =
    *       πLs(
    *         for {
    *           _ <- IO.unit
    *           _ <- IO.sleep(13L.seconds)
    *           _ <- _υ1υ
    *         } yield ()
    *       ).πparSequence
    *     _υ1υ
    *   }
    *   _    <- _υ1υ
    * } yield ()
    * }}}
    */
  test("replication - output - no parallelism & nonempty pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(-1, Some(13L->"seconds"), None, ∅())

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(Term.Name(_)),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Val(Mod.Lazy() :: Nil,
                                                           Pat.Var(Term.Name(_)) :: Nil,
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.Select(
                                                             Term.Apply(
                                                               Term.Name("πLs"),
                                                               List(
                                                                 Term.ForYield(List(
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(`\\`), Term.Name("unit"))),
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("sleep")),
                                                                                                                                 Term.Select(Lit.Long(13L), Term.Name("seconds")) :: Nil)),
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Name(_))
                                                                               ),
                                                                               Lit.Unit())
                                                               )
                                                             ),
                                                             Term.Name("πparSequence")
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(), Term.Name(_))
      ) => true
    }
  }

  /**
    * `! 13* 13,seconds`
    *
    * @example {{{
    * for {
    *   _υ2υ <- Semaphore[IO](13)
    *   _υ1υ <- IO {
    *     lazy val _υ1υ: IO[Any] =
    *       πLs(
    *         _υ2υ.release,
    *         for {
    *           _ <- IO.unit
    *           _ <- IO.sleep(13L.seconds)
    *           _ <- _υ2υ.acquire
    *           _ <- _υ1υ
    *         } yield ()
    *       ).πparSequence
    *     _υ1υ
    *   }
    *   _    <- _υ2υ.acquire
    *   _    <- _υ1υ
    * } yield ()
    * }}}
    */
  test("replication - output - parallelism >= 0 & nonempty pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(13, Some(13L->"seconds"), None, ∅())

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
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.Select(
                                                             Term.Apply(
                                                               Term.Name("πLs"),
                                                               List(
                                                                 Term.Select(Term.Name(_), Term.Name("release")),
                                                                 Term.ForYield(List(
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(`\\`), Term.Name("unit"))),
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("sleep")),
                                                                                                                                 Term.Select(Lit.Long(13L), Term.Name("seconds")) :: Nil)),
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Name(_))
                                                                               ),
                                                                               Lit.Unit())
                                                               )
                                                             ),
                                                             Term.Name("πparSequence")
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                Enumerator.Generator(Pat.Wildcard(), Term.Name(_))
      ) => true
    }
  }

  /////////////////////////////////////////////////// replication (unguarded) //


object ProgramSuite:

  val \ = "IO"
