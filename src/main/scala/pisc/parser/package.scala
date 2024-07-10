package pisc

package object parser:

  import Calculus.{ `+`, ∅, `?:`, `(*)`, `!`, π, τ }

  type `-` = ∅.type | `?:` | `(*)` | `!`

  type `&` = `+` | `-`

  type μ = π | τ
