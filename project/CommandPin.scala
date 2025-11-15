import sbt._
import Keys._

import complete.DefaultParsers._

import internal.util.complete._

import java.io.File
import java.nio.file.Path

object CommandPin {

  def pin = Command("pin") { state =>
    import state._

    val extracted = Project.extract(state)
    import extracted._

    // https://stackoverflow.com/questions/30653900/sbt-parsers-and-dynamic-completions

    val dir = extracted.get(baseDirectory).getPath.toString

    def examples(path: String, prefix: String) = {
      val fileExamples = new FileExamples(new File(Path.of(dir, "examples", path).toString), prefix)
      fileExamples().map(_.stripSuffix(".pisc")).toSeq
    }

    val opts = Map("-ca" -> Nil, "-ca" -> Nil, "-kk" -> (0 to 2).map("-O" + _))

    def suggestions(args: Seq[String]): Seq[String] =
      args.flatMap {
        case it if it.startsWith("-") && args.size == 1 =>
          Seq("-ca", "-ce", "-kk").filter(_.startsWith(it.toLowerCase))
        case it =>
          { if (it.isEmpty && args.size == 1) suggestions(Seq("-")) else Nil } ++
          { if (args.size > 1 && opts.contains(args(1))) opts(args(1)) else Nil } ++
          examples("test", it) ++ { if (it.startsWith("test")) Nil else examples("pisc", it) }
      }

    val pinArgsParser: Parser[Seq[String]] = {

      def loop(previous: Seq[String]): Parser[Seq[String]] = {
        token(Space) ~> NotSpace.examples(suggestions(previous): _*).flatMap(res => loop(previous :+ res))
      }.?.map(_.getOrElse(previous))

      loop(Seq("")).map(_.tail)

    }

    pinArgsParser
  } { (state, pisc) =>

    val extracted = Project.extract(state)
    import extracted._

    //https://blog.michal.pawlik.dev/posts/til/sbt-task-with-custom-settings/

    val runKey = currentRef / Compile / run

    val (overrideSettings, runArguments) =
      pisc.headOption.map(_.toLowerCase) match {
        case Some("-kk") =>
          Seq(currentRef / Compile / mainClass := Some("pisc.kk.Main")) -> pisc.tail
        case Some("-ca") =>
          Seq(currentRef / Compile / mainClass := Some("pisc.ca.Main")) -> pisc.tail
        case Some("-ce") =>
          Seq(currentRef / Compile / mainClass := Some("pisc.ce.Main")) -> pisc.tail
        case _ =>
          Seq(currentRef / Compile / mainClass := Some("pisc.ce.Main")) -> pisc
      }

    val runState = appendWithSession(overrideSettings, state)

    Project
      .extract(runState)
      .runInputTask(runKey, runArguments.mkString(" ", " ", ""), runState)

    state
  }

}
