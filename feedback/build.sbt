libraryDependencies ++= Seq("com.github.japgolly.scalajs-react" %%% "core-bundle-cats_effect",
                            "com.github.japgolly.scalajs-react" %%% "extra",
                            "com.github.japgolly.scalajs-react" %%% "extra-ext-monocle3"
                        ).map(_ % "4.0.0") ++
                        Seq("io.circe" %%% "circe-generic" % "0.14.16",
                            "io.circe" %%% "circe-parser" % "0.14.16",
                            "org.http4s" %%% "http4s-dom" % "0.2.12",
                            "org.http4s" %%% "http4s-circe" % "0.23.37")
