package xibss.attribute

import xibss.view.ViewLike


object ObjectFit {

  val modeMapping = Map (
    "scaleToFill" -> "fill",
    "scaleAspectFit" -> "fit",
    "scaleAspectFill" -> "cover"
  )

  def unapply(view: ViewLike): Option[String] = {
    view.xml.attributes.asAttrMap.get("contentMode").flatMap(a => modeMapping.get(a))
  }

}
