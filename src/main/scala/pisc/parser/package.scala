package pisc

package object parser:

  import Calculus.{ `+`, `𝟎`, `?:`, `()`, `!`, π, τ }

  type `-` = `𝟎`.type | `?:` | `()` | `!`

  type `&` = `+` | `-`

  type μ = π | τ
