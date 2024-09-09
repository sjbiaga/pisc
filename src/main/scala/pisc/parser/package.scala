package pisc

package object parser:

  import Calculus.{ `+`, ∅, `?:`, `(*)`, `!`, π, τ, Pre, AST }
  import StochasticPi.{ Act, Sum }

  type `-` = ∅.type | `?:` | `(*)` | `!`

  type `&` = `+` | `-`

  type μ = π | τ

  type `Pre | AST` = Pre | AST

  type `Act | Sum` = Act | Sum
