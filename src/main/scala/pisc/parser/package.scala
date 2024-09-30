package pisc

package object parser:

  import Calculus.{ `+`, ∅, `?:`, `{}`, `(*)`, `!`, `[|]`, λ, π, τ, Pre, AST }
  import StochasticPi.{ Act, Sum }

  type `-` = ∅.type | `?:` | `{}` | `(*)` | `!` | `[|]`

  type `&` = `+` | `-`

  type μ = π | τ

  type `Pre | AST` = Pre | AST

  type `Act | Sum` = Act | Sum

  type `name | process` = λ | `+`
