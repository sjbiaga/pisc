package basc
package feedback

import scala.scalajs.js

import org.scalajs.dom

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._


object Download:

  case class Props(fileName: String,
                   fileContent: String,
                   mimeType: String = "text/plain")

  val Component = ScalaComponent.builder[Props]("Download")
    .render_P { props =>
      val parts = js.Array[dom.BlobPart](props.fileContent.asInstanceOf[dom.BlobPart])

      val blobOptions = js.Dynamic.literal(`type` = props.mimeType).asInstanceOf[dom.BlobPropertyBag]
      val blob = new dom.Blob(parts, blobOptions)

      val blobUrl = dom.URL.createObjectURL(blob)

      <.a(
        ^.href := blobUrl,
        ^.download := props.fileName,
        "⤓"
      )
    }
    .build

  def apply(fileName: String, fileContent: String, mimeType: String = "text/plain") =
    Component(Props(fileName, fileContent, mimeType))
