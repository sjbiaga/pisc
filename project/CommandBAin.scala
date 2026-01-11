import sbt._
import Keys._

import complete.DefaultParsers._

import internal.util.complete._

import java.io.File
import java.nio.file.Path

object CommandBAin {

  def bain = Command("bain") { state =>
    import state._

    val extracted = Project.extract(state)
    import extracted._

    // https://stackoverflow.com/questions/30653900/sbt-parsers-and-dynamic-completions

    val dir = extracted.get(baseDirectory).getPath.toString

    def examples(path: String, prefix: String) = {
      val fileExamples = new FileExamples(new File(Path.of(dir, "examples", path).toString), prefix)
      fileExamples().map(_.stripSuffix(".basc")).toSeq
    }

    val opts = Map("-ce" -> Nil, "-cef" -> Nil, "-fs2" -> Seq("cats.effect.IO", "zio.Task").map("-F" + _), "-zs" -> Nil)

    def suggestions(args: Seq[String]): Seq[String] =
      args.flatMap {
        case it if it.startsWith("-") && args.size == 1 =>
          Seq("-ce", "-cef", "-fs2", "-zs").filter(_.startsWith(it.toLowerCase))
        case it =>
          { if (it.isEmpty && args.size == 1) suggestions(Seq("-")) else Nil } ++
          { if (args.size > 1 && opts.contains(args(1))) opts(args(1)) else Nil } ++
          examples("test", it) ++ { if (it.startsWith("test")) Nil else examples("basc", it) }
      }

    val bainArgsParser: Parser[Seq[String]] = {

      def loop(previous: Seq[String]): Parser[Seq[String]] = {
        token(Space) ~> NotSpace.examples(suggestions(previous): _*).flatMap(res => loop(previous :+ res))
      }.?.map(_.getOrElse(previous))

      loop(Seq("")).map(_.tail)

    }

    bainArgsParser
  } { (state, basc) =>

    val extracted = Project.extract(state)
    import extracted._

    //https://blog.michal.pawlik.dev/posts/til/sbt-task-with-custom-settings/

    val runKey = currentRef / Compile / run

    val (overrideSettings, runArguments) =
      basc.headOption.map(_.toLowerCase) match {
        case Some(it) if it.head == '-' =>
          Seq(currentRef / Compile / mainClass := Some(s"basc.${it.tail}.Main")) -> basc.tail
        case _ =>
          Seq(currentRef / Compile / mainClass := Some("basc.ce.Main")) -> basc
      }

    val runState = appendWithSession(overrideSettings, state)

    Project
      .extract(runState)
      .runInputTask(runKey, runArguments.mkString(" ", " ", ""), runState)

    state
  }

}
