package pisc

package object parser:

  import Calculus.{ `+`, ∅, `?:`, `(*)`, `!`, π, τ, Pre, AST }

  type `-` = ∅.type | `?:` | `(*)` | `!`

  type `&` = `+` | `-`

  type μ = π | τ

  type `Pre | AST` = Pre | AST
