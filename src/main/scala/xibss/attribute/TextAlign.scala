package xibss.attribute

import xibss.view.ViewLike


object TextAlign {

  val l = List("left", "center", "right")
  var read:(ViewLike) => Option[String] = { _ => None }

  def unapply(view: ViewLike): Option[String] = {
    val native = view.xml.attributes.asAttrMap.get("textAlignment").headOption
    List(read(view), native).flatten.filter(l.contains(_)).headOption
  }

}