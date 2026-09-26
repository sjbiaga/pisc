package basc
package feedback

import _root_.io.circe.Codec


package object analytics:

  enum Plugin derives Codec.AsObject:
    case syncRate(rate: Option[BigDecimal])
    case probability(probability: BigDecimal)
    case whatIf(factor: BigDecimal, term: Option[BigDecimal])

  def time(milliseconds: Long): Option[String] =
    val seconds = milliseconds / 1000
    val minutes = seconds / 60
    val hours = minutes / 60
    val days = hours / 24

    if seconds > 0
    then
      if minutes > 0
      then
        if hours > 0
        then
          if days > 0
          then
            Some(s"$days DAYS ${hours%24}:${minutes%60}:${seconds%60}")
          else
            Some(s"${hours%24}:${minutes%60}:${seconds%60}")
        else
          Some(s"${minutes%60}:${seconds%60}")
      else
        Some(s"${seconds%60}")
    else
      None
