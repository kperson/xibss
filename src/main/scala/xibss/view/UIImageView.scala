package xibss.view

import xibss.ViewNode


case class UIImageView(node: ViewNode) extends ViewLike {

  def tag = "xibss-image"
  def tagIsContainer = false

  def image: Option[String] = node.xml.attributes.asAttrMap.get("image")

  override def extraAttributes: Map[String, Option[String]] = {
    Map("image" -> image)
  }

}
