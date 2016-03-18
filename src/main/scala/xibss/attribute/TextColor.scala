package xibss.attribute

import xibss._
import xibss.view.ViewLike

import scala.xml.Node


object TextColor {

  var read:(ViewLike) => Option[Color] = { _ => None }

  def unapply(view: ViewLike): Option[Color] = {
    val native = (view.xml \ "color").filter { _.attributes.asAttrMap.get("key") == Some("textColor") }.headOption.flatMap { c =>
      RGBColor.unapply(c)
    }
    List(read(view), native).flatMap { a => a }.headOption
  }

}