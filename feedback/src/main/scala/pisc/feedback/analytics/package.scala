package pisc
package feedback


package object analytics:

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
