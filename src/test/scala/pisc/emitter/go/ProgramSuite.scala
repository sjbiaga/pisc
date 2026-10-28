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

  test("replication - bound output guard - no parallelism & no pace") {

    val `13` = `!`(-1, None, Some(π(λ(Symbol("guard")), λ(Symbol("name")), Some("ν"), None)), ∅())

    assertEquals(`13`.emit.toString,
"""var _υ1υ func (Go_NamePi) Go_TrampolinePi[Go_NamePi]
_υ1υ = (name Go_NamePi) Go_TrampolinePi[Go_NamePi] => {
    bind _ = Go_SpawnPi(() => {
        Go_returnPi
    })
    bind name = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
    bind _ = guard match {
        case Go_QueuePi(it) => it.Go_EnqueuePi(go_context, name)
        case it => Panic(it)
    }
    _υ1υ(name)
}
bind name = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
bind _ = guard match {
    case Go_QueuePi(it) => it.Go_EnqueuePi(go_context, name)
    case it => Panic(it)
}
_υ1υ(name)"""
    )
  }

  test("replication - bound output guard - parallelism >= 0 & no pace") {

    val `13` = `!`(13, None, Some(π(λ(Symbol("guard")), λ(Symbol("name")), Some("ν"), None)), ∅())

    assertEquals(`13`.emit.toString,
"""val _υ2υ = Go_NewSemaphorePi(13)
var _υ1υ func (Go_NamePi) Go_TrampolinePi[Go_NamePi]
_υ1υ = (name Go_NamePi) Go_TrampolinePi[Go_NamePi] => {
    bind _ = _υ2υ.Acquire1(go_context)
    bind _ = Go_Spawn_ReleasablePi(_υ2υ, () => {
        Go_returnPi
    })
    bind name = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
    bind _ = guard match {
        case Go_QueuePi(it) => it.Go_EnqueuePi(go_context, name)
        case it => Panic(it)
    }
    _υ1υ(name)
}
bind name = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
bind _ = guard match {
    case Go_QueuePi(it) => it.Go_EnqueuePi(go_context, name)
    case it => Panic(it)
}
_υ1υ(name)"""
    )
  }

  test("replication - bound output guard - no parallelism & nonempty pace") {

    val `13` = `!`(-1, Some(13L->"Second"), Some(π(λ(Symbol("guard")), λ(Symbol("name")), Some("ν"), None)), ∅())

    assertEquals(`13`.emit.toString,
"""var _υ1υ func (Go_NamePi) Go_TrampolinePi[Go_NamePi]
_υ1υ = (name Go_NamePi) Go_TrampolinePi[Go_NamePi] => {
    bind _ = Go_SpawnPi(() => {
        Go_returnPi
    })
    bind _ = Go_PacePi(13, time.Second)
    bind name = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
    bind _ = guard match {
        case Go_QueuePi(it) => it.Go_EnqueuePi(go_context, name)
        case it => Panic(it)
    }
    _υ1υ(name)
}
bind name = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
bind _ = guard match {
    case Go_QueuePi(it) => it.Go_EnqueuePi(go_context, name)
    case it => Panic(it)
}
_υ1υ(name)"""
    )
  }

  test("replication - bound output guard - parallelism >= 0 & nonempty pace") {

    val `13` = `!`(13, Some(13L->"Second"), Some(π(λ(Symbol("guard")), λ(Symbol("name")), Some("ν"), None)), ∅())

    assertEquals(`13`.emit.toString,
"""val _υ2υ = Go_NewSemaphorePi(13)
var _υ1υ func (Go_NamePi) Go_TrampolinePi[Go_NamePi]
_υ1υ = (name Go_NamePi) Go_TrampolinePi[Go_NamePi] => {
    bind _ = _υ2υ.Acquire1(go_context)
    bind _ = Go_Spawn_ReleasablePi(_υ2υ, () => {
        Go_returnPi
    })
    bind _ = Go_PacePi(13, time.Second)
    bind name = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
    bind _ = guard match {
        case Go_QueuePi(it) => it.Go_EnqueuePi(go_context, name)
        case it => Panic(it)
    }
    _υ1υ(name)
}
bind name = Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))
bind _ = guard match {
    case Go_QueuePi(it) => it.Go_EnqueuePi(go_context, name)
    case it => Panic(it)
}
_υ1υ(name)"""
    )
  }

  //////////////////////////////////////////////// replication (bound output) //

  // REPLICATION (INPUT) ///////////////////////////////////////////////////////

  test("replication - input guard - no parallelism & no pace") {

    val `13` = `!`(-1, None, Some(π(λ(Symbol("guard")), λ(Symbol("name")), Some(""), None)), ∅())

    assertEquals(`13`.emit.toString,
"""var _υ1υ func (Go_NamePi) Go_TrampolinePi[Go_NamePi]
_υ1υ = (name Go_NamePi) Go_TrampolinePi[Go_NamePi] => {
    bind _ = Go_SpawnPi(() => {
        Go_returnPi
    })
    bind name = guard match {
        case Go_QueuePi(it) => it.Go_DequeuePi(go_context)
        case it => Panic(it)
    }
    _υ1υ(name)
}
bind name = guard match {
    case Go_QueuePi(it) => it.Go_DequeuePi(go_context)
    case it => Panic(it)
}
_υ1υ(name)"""
    )
  }

  test("replication - input guard - parallelism >= 0 & no pace") {

    val `13` = `!`(13, None, Some(π(λ(Symbol("guard")), λ(Symbol("name")), Some(""), None)), ∅())

    assertEquals(`13`.emit.toString,
"""val _υ2υ = Go_NewSemaphorePi(13)
var _υ1υ func (Go_NamePi) Go_TrampolinePi[Go_NamePi]
_υ1υ = (name Go_NamePi) Go_TrampolinePi[Go_NamePi] => {
    bind _ = _υ2υ.Acquire1(go_context)
    bind _ = Go_Spawn_ReleasablePi(_υ2υ, () => {
        Go_returnPi
    })
    bind name = guard match {
        case Go_QueuePi(it) => it.Go_DequeuePi(go_context)
        case it => Panic(it)
    }
    _υ1υ(name)
}
bind name = guard match {
    case Go_QueuePi(it) => it.Go_DequeuePi(go_context)
    case it => Panic(it)
}
_υ1υ(name)"""
    )
  }

  test("replication - input guard - no parallelism & nonempty pace") {

    val `13` = `!`(-1, Some(13L->"Second"), Some(π(λ(Symbol("guard")), λ(Symbol("name")), Some(""), None)), ∅())

    assertEquals(`13`.emit.toString,
"""var _υ1υ func (Go_NamePi) Go_TrampolinePi[Go_NamePi]
_υ1υ = (name Go_NamePi) Go_TrampolinePi[Go_NamePi] => {
    bind _ = Go_SpawnPi(() => {
        Go_returnPi
    })
    bind _ = Go_PacePi(13, time.Second)
    bind name = guard match {
        case Go_QueuePi(it) => it.Go_DequeuePi(go_context)
        case it => Panic(it)
    }
    _υ1υ(name)
}
bind name = guard match {
    case Go_QueuePi(it) => it.Go_DequeuePi(go_context)
    case it => Panic(it)
}
_υ1υ(name)"""
    )
  }

  test("replication - input guard - parallelism >= 0 & nonempty pace") {

    val `13` = `!`(13, Some(13L->"Second"), Some(π(λ(Symbol("guard")), λ(Symbol("name")), Some(""), None)), ∅())

    assertEquals(`13`.emit.toString,
"""val _υ2υ = Go_NewSemaphorePi(13)
var _υ1υ func (Go_NamePi) Go_TrampolinePi[Go_NamePi]
_υ1υ = (name Go_NamePi) Go_TrampolinePi[Go_NamePi] => {
    bind _ = _υ2υ.Acquire1(go_context)
    bind _ = Go_Spawn_ReleasablePi(_υ2υ, () => {
        Go_returnPi
    })
    bind _ = Go_PacePi(13, time.Second)
    bind name = guard match {
        case Go_QueuePi(it) => it.Go_DequeuePi(go_context)
        case it => Panic(it)
    }
    _υ1υ(name)
}
bind name = guard match {
    case Go_QueuePi(it) => it.Go_DequeuePi(go_context)
    case it => Panic(it)
}
_υ1υ(name)"""
    )
  }

  /////////////////////////////////////////////////////// replication (input) //

  // REPLICATION (OUTPUT) //////////////////////////////////////////////////////

  test("replication - output guard - no parallelism & no pace") {

    val `13` = `!`(-1, None, Some(π(λ(Symbol("guard")), λ(Symbol("guard")), None, None)), ∅())

    assertEquals(`13`.emit.toString,
"""var _υ1υ func () Go_TrampolinePi[Go_NamePi]
_υ1υ = () Go_TrampolinePi[Go_NamePi] => {
    bind _ = Go_SpawnPi(() => {
        Go_returnPi
    })
    bind _ = guard match {
        case Go_QueuePi(it) => it.Go_EnqueuePi(go_context, guard)
        case it => Panic(it)
    }
    _υ1υ()
}
bind _ = guard match {
    case Go_QueuePi(it) => it.Go_EnqueuePi(go_context, guard)
    case it => Panic(it)
}
_υ1υ()"""
    )
  }

  test("replication - output guard - parallelism >= 0 & no pace") {

    val `13` = `!`(13, None, Some(π(λ(Symbol("guard")), λ(Symbol("guard")), None, None)), ∅())

    assertEquals(`13`.emit.toString,
"""val _υ2υ = Go_NewSemaphorePi(13)
var _υ1υ func () Go_TrampolinePi[Go_NamePi]
_υ1υ = () Go_TrampolinePi[Go_NamePi] => {
    bind _ = _υ2υ.Acquire1(go_context)
    bind _ = Go_Spawn_ReleasablePi(_υ2υ, () => {
        Go_returnPi
    })
    bind _ = guard match {
        case Go_QueuePi(it) => it.Go_EnqueuePi(go_context, guard)
        case it => Panic(it)
    }
    _υ1υ()
}
bind _ = guard match {
    case Go_QueuePi(it) => it.Go_EnqueuePi(go_context, guard)
    case it => Panic(it)
}
_υ1υ()"""
    )
  }

  test("replication - output guard - no parallelism & nonempty pace") {

    val `13` = `!`(-1, Some(13L->"Second"), Some(π(λ(Symbol("guard")), λ(Symbol("guard")), None, None)), ∅())

    assertEquals(`13`.emit.toString,
"""var _υ1υ func () Go_TrampolinePi[Go_NamePi]
_υ1υ = () Go_TrampolinePi[Go_NamePi] => {
    bind _ = Go_SpawnPi(() => {
        Go_returnPi
    })
    bind _ = Go_PacePi(13, time.Second)
    bind _ = guard match {
        case Go_QueuePi(it) => it.Go_EnqueuePi(go_context, guard)
        case it => Panic(it)
    }
    _υ1υ()
}
bind _ = guard match {
    case Go_QueuePi(it) => it.Go_EnqueuePi(go_context, guard)
    case it => Panic(it)
}
_υ1υ()"""
    )
  }

  test("replication - output guard - parallelism >= 0 & nonempty pace") {

    val `13` = `!`(13, Some(13L->"Second"), Some(π(λ(Symbol("guard")), λ(Symbol("guard")), None, None)), ∅())

    assertEquals(`13`.emit.toString,
"""val _υ2υ = Go_NewSemaphorePi(13)
var _υ1υ func () Go_TrampolinePi[Go_NamePi]
_υ1υ = () Go_TrampolinePi[Go_NamePi] => {
    bind _ = _υ2υ.Acquire1(go_context)
    bind _ = Go_Spawn_ReleasablePi(_υ2υ, () => {
        Go_returnPi
    })
    bind _ = Go_PacePi(13, time.Second)
    bind _ = guard match {
        case Go_QueuePi(it) => it.Go_EnqueuePi(go_context, guard)
        case it => Panic(it)
    }
    _υ1υ()
}
bind _ = guard match {
    case Go_QueuePi(it) => it.Go_EnqueuePi(go_context, guard)
    case it => Panic(it)
}
_υ1υ()"""
    )
  }

  ////////////////////////////////////////////////////// replication (output) //

  // REPLICATION (UNGUARDED) ///////////////////////////////////////////////////

  test("replication - unguarded - no parallelism & no pace") {

    val `13` = `!`(-1, None, None, ∅())

    assertEquals(`13`.emit.toString,
"""var _υ1υ func () Go_TrampolinePi[Go_NamePi]
_υ1υ = () Go_TrampolinePi[Go_NamePi] => {
    bind _ = Go_SpawnPi(() => {
        Go_returnPi
    })
    _υ1υ()
}
_υ1υ()"""
    )
  }

  test("replication - unguarded - parallelism >= 0 & no pace") {

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
_υ1υ()"""
    )
  }

  test("replication - unguarded - no parallelism & nonempty pace") {

    val `13` = `!`(-1, Some(13L->"Second"), None, ∅())

    assertEquals(`13`.emit.toString,
"""var _υ1υ func () Go_TrampolinePi[Go_NamePi]
_υ1υ = () Go_TrampolinePi[Go_NamePi] => {
    bind _ = Go_SpawnPi(() => {
        Go_returnPi
    })
    bind _ = Go_PacePi(13, time.Second)
    _υ1υ()
}
_υ1υ()"""
    )
  }

  test("replication - unguarded - parallelism >= 0 & nonempty pace") {

    val `13` = `!`(13, Some(13L->"Second"), None, ∅())

    assertEquals(`13`.emit.toString,
"""val _υ2υ = Go_NewSemaphorePi(13)
var _υ1υ func () Go_TrampolinePi[Go_NamePi]
_υ1υ = () Go_TrampolinePi[Go_NamePi] => {
    bind _ = _υ2υ.Acquire1(go_context)
    bind _ = Go_Spawn_ReleasablePi(_υ2υ, () => {
        Go_returnPi
    })
    bind _ = Go_PacePi(13, time.Second)
    _υ1υ()
}
_υ1υ()"""
    )
  }

  /////////////////////////////////////////////////// replication (unguarded) //


object ProgramSuite:

  given (Int => (Int, Int)) = { _ => (0, 0) }
