package pisc

  package object parser:

    import Calculus.{ `+`, `𝟎`, `?:`, `()`, `!` }

    type `-` = `𝟎`.type | `?:` | `()` | `!`

    type `&` = `+` | `-`
