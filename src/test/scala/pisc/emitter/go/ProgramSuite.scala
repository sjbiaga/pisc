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
package go

import munit.FunSuite

import parser.Calculus.*
import Program.emit
import ProgramSuite.*
import ProgramSuite.given


class ProgramSuite extends FunSuite:

  // REPLICATION (BOUND OUTPUT) ////////////////////////////////////////////////

  /**
    * `! .guard<νname1,name2>.`
    */
  test("replication - bound output guard - no parallelism & no pace") {
    implicit def id: String = "_υ1υ"

    val `13` = `!`(-1, None, Some(π(λ(Symbol("guard")), Some("ν"), None, λ(Symbol("name1")), λ(Symbol("name2")))), ∅())

    assertEquals(`13`.emit.toString,
"""var _υ1υ func (Go_NamePi, Go_NamePi) Go_TrampolinePi[Go_NamePi]
_υ1υ = (name1 Go_NamePi, name2 Go_NamePi) Go_TrampolinePi[Go_NamePi] => {
    bind _ = Go_SpawnPi(() => {
        Go_returnPi
    })
    bind name1 = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
    bind name2 = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
    bind _ = guard match {
        case Go_QueuePi(it) => it.Go_EnqueuePi(Go_NamesPi(name1, name2))
        case it => Panic(it)
    }
    _υ1υ(name1, name2)
}
bind name1 = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
bind name2 = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
bind _ = guard match {
    case Go_QueuePi(it) => it.Go_EnqueuePi(Go_NamesPi(name1, name2))
    case it => Panic(it)
}
_υ1υ(name1, name2)""")
  }

  /**
    * `! 13* .guard<νname1,name2>.`
    */
  test("replication - bound output guard - parallelism >= 0 & no pace") {
    implicit def id: String = "_υ1υ"

    val `13` = `!`(13, None, Some(π(λ(Symbol("guard")), Some("ν"), None, λ(Symbol("name1")), λ(Symbol("name2")))), ∅())

    assertEquals(`13`.emit.toString,
"""val _υ2υ = Go_NewSemaphorePi(13)
var _υ1υ func (Go_NamePi, Go_NamePi) Go_TrampolinePi[Go_NamePi]
_υ1υ = (name1 Go_NamePi, name2 Go_NamePi) Go_TrampolinePi[Go_NamePi] => {
    bind _ = _υ2υ.Acquire1(go_context)
    bind _ = Go_Spawn_ReleasablePi(_υ2υ, () => {
        Go_returnPi
    })
    bind name1 = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
    bind name2 = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
    bind _ = guard match {
        case Go_QueuePi(it) => it.Go_EnqueuePi(Go_NamesPi(name1, name2))
        case it => Panic(it)
    }
    _υ1υ(name1, name2)
}
bind name1 = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
bind name2 = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
bind _ = guard match {
    case Go_QueuePi(it) => it.Go_EnqueuePi(Go_NamesPi(name1, name2))
    case it => Panic(it)
}
_υ1υ(name1, name2)""")
  }

  /**
    * `! 13,seconds .guard<νname1,name2>.`
    */
  test("replication - bound output guard - no parallelism & nonempty pace") {
    implicit def id: String = "_υ1υ"

    val `13` = `!`(-1, Some(13L->"seconds"), Some(π(λ(Symbol("guard")), Some("ν"), None, λ(Symbol("name1")), λ(Symbol("name2")))), ∅())

    assertEquals(`13`.emit.toString,
"""var _υ1υ func (Go_NamePi, Go_NamePi) Go_TrampolinePi[Go_NamePi]
_υ1υ = (name1 Go_NamePi, name2 Go_NamePi) Go_TrampolinePi[Go_NamePi] => {
    bind _ = Go_SpawnPi(() => {
        Go_returnPi
    })
    bind _ = Go_PacePi(13, time.seconds)
    bind name1 = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
    bind name2 = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
    bind _ = guard match {
        case Go_QueuePi(it) => it.Go_EnqueuePi(Go_NamesPi(name1, name2))
        case it => Panic(it)
    }
    _υ1υ(name1, name2)
}
bind name1 = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
bind name2 = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
bind _ = guard match {
    case Go_QueuePi(it) => it.Go_EnqueuePi(Go_NamesPi(name1, name2))
    case it => Panic(it)
}
_υ1υ(name1, name2)""")
  }

  /**
    * `! 13* 13,seconds .guard<νname1,name2>.`
    */
  test("replication - bound output guard - parallelism >= 0 & nonempty pace") {
    implicit def id: String = "_υ1υ"

    val `13` = `!`(13, Some(13L->"seconds"), Some(π(λ(Symbol("guard")), Some("ν"), None, λ(Symbol("name1")), λ(Symbol("name2")))), ∅())

    assertEquals(`13`.emit.toString,
"""val _υ2υ = Go_NewSemaphorePi(13)
var _υ1υ func (Go_NamePi, Go_NamePi) Go_TrampolinePi[Go_NamePi]
_υ1υ = (name1 Go_NamePi, name2 Go_NamePi) Go_TrampolinePi[Go_NamePi] => {
    bind _ = _υ2υ.Acquire1(go_context)
    bind _ = Go_Spawn_ReleasablePi(_υ2υ, () => {
        Go_returnPi
    })
    bind _ = Go_PacePi(13, time.seconds)
    bind name1 = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
    bind name2 = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
    bind _ = guard match {
        case Go_QueuePi(it) => it.Go_EnqueuePi(Go_NamesPi(name1, name2))
        case it => Panic(it)
    }
    _υ1υ(name1, name2)
}
bind name1 = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
bind name2 = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
bind _ = guard match {
    case Go_QueuePi(it) => it.Go_EnqueuePi(Go_NamesPi(name1, name2))
    case it => Panic(it)
}
_υ1υ(name1, name2)""")
  }

  //////////////////////////////////////////////// replication (bound output) //

  // REPLICATION (INPUT) ///////////////////////////////////////////////////////

  /**
    * `! .guard(name1,name2).`
    */
  test("replication - input guard - no parallelism & no pace") {
    implicit def id: String = "_υ1υ"

    val `13` = `!`(-1, None, Some(π(λ(Symbol("guard")), Some(""), None, λ(Symbol("name1")), λ(Symbol("name2")))), ∅())

    assertEquals(`13`.emit.toString,
"""var _υ1υ func (Go_NamePi, Go_NamePi) Go_TrampolinePi[Go_NamePi]
_υ1υ = (name1 Go_NamePi, name2 Go_NamePi) Go_TrampolinePi[Go_NamePi] => {
    bind _ = Go_SpawnPi(() => {
        Go_returnPi
    })
    guard match {
        case Go_QueuePi(_υ2υ) => {
            bind _υ2υ = _υ2υ.Go_DequeuePi()
            _υ2υ.ToList() match {
                case Cons(name1, Cons(name2, Nil())) => {
                    _υ1υ(name1, name2)
                }
                case it => Panic(it)
            }
        }
        case it => Panic(it)
    }
}
guard match {
    case Go_QueuePi(_υ3υ) => {
        bind _υ3υ = _υ3υ.Go_DequeuePi()
        _υ3υ.ToList() match {
            case Cons(name1, Cons(name2, Nil())) => {
                _υ1υ(name1, name2)
            }
            case it => Panic(it)
        }
    }
    case it => Panic(it)
}""")
  }

  /**
    * `! 13* .guard(name1,name2).`
    */
  test("replication - input guard - parallelism >= 0 & no pace") {
    implicit def id: String = "_υ1υ"

    val `13` = `!`(13, None, Some(π(λ(Symbol("guard")), Some(""), None, λ(Symbol("name1")), λ(Symbol("name2")))), ∅())

    assertEquals(`13`.emit.toString,
"""val _υ2υ = Go_NewSemaphorePi(13)
var _υ1υ func (Go_NamePi, Go_NamePi) Go_TrampolinePi[Go_NamePi]
_υ1υ = (name1 Go_NamePi, name2 Go_NamePi) Go_TrampolinePi[Go_NamePi] => {
    bind _ = _υ2υ.Acquire1(go_context)
    bind _ = Go_Spawn_ReleasablePi(_υ2υ, () => {
        Go_returnPi
    })
    guard match {
        case Go_QueuePi(_υ3υ) => {
            bind _υ3υ = _υ3υ.Go_DequeuePi()
            _υ3υ.ToList() match {
                case Cons(name1, Cons(name2, Nil())) => {
                    _υ1υ(name1, name2)
                }
                case it => Panic(it)
            }
        }
        case it => Panic(it)
    }
}
guard match {
    case Go_QueuePi(_υ4υ) => {
        bind _υ4υ = _υ4υ.Go_DequeuePi()
        _υ4υ.ToList() match {
            case Cons(name1, Cons(name2, Nil())) => {
                _υ1υ(name1, name2)
            }
            case it => Panic(it)
        }
    }
    case it => Panic(it)
}""")
  }

  /**
    * `! 13,seconds .guard(name1,name2).`
    */
  test("replication - input guard - no parallelism & nonempty pace") {
    implicit def id: String = "_υ1υ"

    val `13` = `!`(-1, Some(13L->"seconds"), Some(π(λ(Symbol("guard")), Some(""), None, λ(Symbol("name1")), λ(Symbol("name2")))), ∅())

    assertEquals(`13`.emit.toString,
"""var _υ1υ func (Go_NamePi, Go_NamePi) Go_TrampolinePi[Go_NamePi]
_υ1υ = (name1 Go_NamePi, name2 Go_NamePi) Go_TrampolinePi[Go_NamePi] => {
    bind _ = Go_SpawnPi(() => {
        Go_returnPi
    })
    bind _ = Go_PacePi(13, time.seconds)
    guard match {
        case Go_QueuePi(_υ2υ) => {
            bind _υ2υ = _υ2υ.Go_DequeuePi()
            _υ2υ.ToList() match {
                case Cons(name1, Cons(name2, Nil())) => {
                    _υ1υ(name1, name2)
                }
                case it => Panic(it)
            }
        }
        case it => Panic(it)
    }
}
guard match {
    case Go_QueuePi(_υ3υ) => {
        bind _υ3υ = _υ3υ.Go_DequeuePi()
        _υ3υ.ToList() match {
            case Cons(name1, Cons(name2, Nil())) => {
                _υ1υ(name1, name2)
            }
            case it => Panic(it)
        }
    }
    case it => Panic(it)
}""")
  }

  /**
    * `! 13* 13,seconds .guard(name1,name2).`
    */
  test("replication - input guard - parallelism >= 0 & nonempty pace") {
    implicit def id: String = "_υ1υ"

    val `13` = `!`(13, Some(13L->"seconds"), Some(π(λ(Symbol("guard")), Some(""), None, λ(Symbol("name1")), λ(Symbol("name2")))), ∅())

    assertEquals(`13`.emit.toString,
"""val _υ2υ = Go_NewSemaphorePi(13)
var _υ1υ func (Go_NamePi, Go_NamePi) Go_TrampolinePi[Go_NamePi]
_υ1υ = (name1 Go_NamePi, name2 Go_NamePi) Go_TrampolinePi[Go_NamePi] => {
    bind _ = _υ2υ.Acquire1(go_context)
    bind _ = Go_Spawn_ReleasablePi(_υ2υ, () => {
        Go_returnPi
    })
    bind _ = Go_PacePi(13, time.seconds)
    guard match {
        case Go_QueuePi(_υ3υ) => {
            bind _υ3υ = _υ3υ.Go_DequeuePi()
            _υ3υ.ToList() match {
                case Cons(name1, Cons(name2, Nil())) => {
                    _υ1υ(name1, name2)
                }
                case it => Panic(it)
            }
        }
        case it => Panic(it)
    }
}
guard match {
    case Go_QueuePi(_υ4υ) => {
        bind _υ4υ = _υ4υ.Go_DequeuePi()
        _υ4υ.ToList() match {
            case Cons(name1, Cons(name2, Nil())) => {
                _υ1υ(name1, name2)
            }
            case it => Panic(it)
        }
    }
    case it => Panic(it)
}""")
  }

  /////////////////////////////////////////////////////// replication (input) //

  // REPLICATION (OUTPUT) //////////////////////////////////////////////////////

  /**
    * `! .guard<guard,guard>.`
    */
  test("replication - output guard - no parallelism & no pace") {
    implicit def id: String = "_υ1υ"

    val `13` = `!`(-1, None, Some(π(λ(Symbol("guard")), None, None, λ(Symbol("guard")), λ(Symbol("guard")))), ∅())

    assertEquals(`13`.emit.toString,
"""var _υ1υ func () Go_TrampolinePi[Go_NamePi]
_υ1υ = () Go_TrampolinePi[Go_NamePi] => {
    bind _ = Go_SpawnPi(() => {
        Go_returnPi
    })
    bind _ = guard match {
        case Go_QueuePi(it) => it.Go_EnqueuePi(Go_NamesPi(guard, guard))
        case it => Panic(it)
    }
    _υ1υ()
}
bind _ = guard match {
    case Go_QueuePi(it) => it.Go_EnqueuePi(Go_NamesPi(guard, guard))
    case it => Panic(it)
}
_υ1υ()""")
  }

  /**
    * `! 13* .guard<guard,guard>.`
    */
  test("replication - output guard - parallelism >= 0 & no pace") {
    implicit def id: String = "_υ1υ"

    val `13` = `!`(13, None, Some(π(λ(Symbol("guard")), None, None, λ(Symbol("guard")), λ(Symbol("guard")))), ∅())

    assertEquals(`13`.emit.toString,
"""val _υ2υ = Go_NewSemaphorePi(13)
var _υ1υ func () Go_TrampolinePi[Go_NamePi]
_υ1υ = () Go_TrampolinePi[Go_NamePi] => {
    bind _ = _υ2υ.Acquire1(go_context)
    bind _ = Go_Spawn_ReleasablePi(_υ2υ, () => {
        Go_returnPi
    })
    bind _ = guard match {
        case Go_QueuePi(it) => it.Go_EnqueuePi(Go_NamesPi(guard, guard))
        case it => Panic(it)
    }
    _υ1υ()
}
bind _ = guard match {
    case Go_QueuePi(it) => it.Go_EnqueuePi(Go_NamesPi(guard, guard))
    case it => Panic(it)
}
_υ1υ()""")
  }

  /**
    * `! 13,seconds .guard<guard,guard>.`
    */
  test("replication - output guard - no parallelism & nonempty pace") {
    implicit def id: String = "_υ1υ"

    val `13` = `!`(-1, Some(13L->"seconds"), Some(π(λ(Symbol("guard")), None, None, λ(Symbol("guard")), λ(Symbol("guard")))), ∅())

    assertEquals(`13`.emit.toString,
"""var _υ1υ func () Go_TrampolinePi[Go_NamePi]
_υ1υ = () Go_TrampolinePi[Go_NamePi] => {
    bind _ = Go_SpawnPi(() => {
        Go_returnPi
    })
    bind _ = Go_PacePi(13, time.seconds)
    bind _ = guard match {
        case Go_QueuePi(it) => it.Go_EnqueuePi(Go_NamesPi(guard, guard))
        case it => Panic(it)
    }
    _υ1υ()
}
bind _ = guard match {
    case Go_QueuePi(it) => it.Go_EnqueuePi(Go_NamesPi(guard, guard))
    case it => Panic(it)
}
_υ1υ()""")
  }

  /**
    * `! 13* 13,seconds .guard<guard,guard>.`
    */
  test("replication - output guard - parallelism >= 0 & nonempty pace") {
    implicit def id: String = "_υ1υ"

    val `13` = `!`(13, Some(13L->"seconds"), Some(π(λ(Symbol("guard")), None, None, λ(Symbol("guard")), λ(Symbol("guard")))), ∅())

    assertEquals(`13`.emit.toString,
"""val _υ2υ = Go_NewSemaphorePi(13)
var _υ1υ func () Go_TrampolinePi[Go_NamePi]
_υ1υ = () Go_TrampolinePi[Go_NamePi] => {
    bind _ = _υ2υ.Acquire1(go_context)
    bind _ = Go_Spawn_ReleasablePi(_υ2υ, () => {
        Go_returnPi
    })
    bind _ = Go_PacePi(13, time.seconds)
    bind _ = guard match {
        case Go_QueuePi(it) => it.Go_EnqueuePi(Go_NamesPi(guard, guard))
        case it => Panic(it)
    }
    _υ1υ()
}
bind _ = guard match {
    case Go_QueuePi(it) => it.Go_EnqueuePi(Go_NamesPi(guard, guard))
    case it => Panic(it)
}
_υ1υ()""")
  }

  ////////////////////////////////////////////////////// replication (output) //

  // REPLICATION (UNGUARDED) ///////////////////////////////////////////////////

  /**
    * `!`
    */
  test("replication - unguarded - no parallelism & no pace") {
    implicit def id: String = "_υ1υ"

    val `13` = `!`(-1, None, None, ∅())

    assertEquals(`13`.emit.toString,
"""var _υ1υ func () Go_TrampolinePi[Go_NamePi]
_υ1υ = () Go_TrampolinePi[Go_NamePi] => {
    bind _ = Go_SpawnPi(() => {
        Go_returnPi
    })
    _υ1υ()
}
_υ1υ()""")
  }

  /**
    * `! 13*`
    */
  test("replication - unguarded - parallelism >= 0 & no pace") {
    implicit def id: String = "_υ1υ"

    val `13` = `!`(13, None, None, ∅())

    assertEquals(`13`.emit.toString,
"""val _υ2υ = Go_NewSemaphorePi(13)
var _υ1υ func () Go_TrampolinePi[Go_NamePi]
_υ1υ = () Go_TrampolinePi[Go_NamePi] => {
    bind _ = _υ2υ.Acquire1(go_context)
    bind _ = Go_Spawn_ReleasablePi(_υ2υ, () => {
        Go_returnPi
    })
    _υ1υ()
}
_υ1υ()""")
  }

  /**
    * `! 13,seconds`
    */
  test("replication - unguarded - no parallelism & nonempty pace") {
    implicit def id: String = "_υ1υ"

    val `13` = `!`(-1, Some(13L->"seconds"), None, ∅())

    assertEquals(`13`.emit.toString,
"""var _υ1υ func () Go_TrampolinePi[Go_NamePi]
_υ1υ = () Go_TrampolinePi[Go_NamePi] => {
    bind _ = Go_SpawnPi(() => {
        Go_returnPi
    })
    bind _ = Go_PacePi(13, time.seconds)
    _υ1υ()
}
_υ1υ()""")
  }

  /**
    * `! 13* 13,seconds`
    */
  test("replication - unguarded - parallelism >= 0 & nonempty pace") {
    implicit def id: String = "_υ1υ"

    val `13` = `!`(13, Some(13L->"seconds"), None, ∅())

    assertEquals(`13`.emit.toString,
"""val _υ2υ = Go_NewSemaphorePi(13)
var _υ1υ func () Go_TrampolinePi[Go_NamePi]
_υ1υ = () Go_TrampolinePi[Go_NamePi] => {
    bind _ = _υ2υ.Acquire1(go_context)
    bind _ = Go_Spawn_ReleasablePi(_υ2υ, () => {
        Go_returnPi
    })
    bind _ = Go_PacePi(13, time.seconds)
    _υ1υ()
}
_υ1υ()""")
  }

  /////////////////////////////////////////////////// replication (unguarded) //


object ProgramSuite:

  given (Int => (Int, Int)) = { _ => (0, 0) }
