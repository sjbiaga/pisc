import sbt._
import Keys._

import complete.DefaultParsers._

import internal.util.complete._

import java.io.File
import java.nio.file.Path

object CommandMAin {

  def main = Command("main") { state =>
    import state._

    val extracted = Project.extract(state)
    import extracted._

    // https://stackoverflow.com/questions/30653900/sbt-parsers-and-dynamic-completions

    val dir = extracted.get(baseDirectory).getPath.toString

    def examples(path: String, prefix: String) = {
      val fileExamples = new FileExamples(new File(Path.of(dir, "examples", path).toString), prefix)
      fileExamples().map(_.stripSuffix(".masc")).toSeq
    }

    val opts = Map("-ce" -> Nil, "-fs2" -> Seq("cats.effect.IO", "zio.Task").map("-F" + _), "-monix" -> Seq("cats.effect.IO", "monix.eval.Task").map("-F" + _), "-zs" -> Nil, "-kk" -> (0 to 2).map("-O" + _))

    def suggestions(args: Seq[String]): Seq[String] =
      args.flatMap {
        case it if it.startsWith("-") && args.size == 1 =>
          Seq("-ce", "-kk", "-fs2", "-monix", "-zs").filter(_.startsWith(it.toLowerCase))
        case it =>
          { if (it.isEmpty && args.size == 1) suggestions(Seq("-")) else Nil } ++
          { if (args.size > 1 && opts.contains(args(1))) opts(args(1)) else Nil } ++
          examples("test", it) ++ { if (it.startsWith("test")) Nil else examples("masc", it) }
      }

    val mainArgsParser: Parser[Seq[String]] = {

      def loop(previous: Seq[String]): Parser[Seq[String]] = {
        token(Space) ~> NotSpace.examples(suggestions(previous): _*).flatMap(res => loop(previous :+ res))
      }.?.map(_.getOrElse(previous))

      loop(Seq("")).map(_.tail)

    }

    mainArgsParser
  } { (state, masc) =>

    val extracted = Project.extract(state)
    import extracted._

    //https://blog.michal.pawlik.dev/posts/til/sbt-task-with-custom-settings/

    val runKey = currentRef / Compile / run

    val (overrideSettings, runArguments) =
      masc.headOption.map(_.toLowerCase) match {
        case Some(it) if it.head == '-' =>
          Seq(currentRef / Compile / mainClass := Some(s"masc.${it.tail}.Main")) -> masc.tail
        case _ =>
          Seq(currentRef / Compile / mainClass := Some("masc.ce.Main")) -> masc
      }

    val runState = appendWithSession(overrideSettings, state)

    Project
      .extract(runState)
      .runInputTask(runKey, runArguments.mkString(" ", " ", ""), runState)

    state
  }

}
