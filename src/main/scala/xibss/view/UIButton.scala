package xibss.view

import xibss._
import xibss.parser.XIBParser._

case class UIButton(node: ViewNode) extends ViewLike {

  def tag = "button"
  def tagIsContainer = true

  override def innerHTML: Option[String] = {
    val key = node.xml \ "state"
    val title = (node.xml \ "state" \ "@title").textOptional
    key.filter(_.attributes.asAttrMap.get("key") == Some("normal")).flatMap(_.attributes.asAttrMap.get("title")).headOption
  }

  override def extraGSS() = List(
    "outline: 0",
    "border: none"
  )

}
