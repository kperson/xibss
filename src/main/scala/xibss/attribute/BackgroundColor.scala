package xibss.attribute

import xibss._
import xibss.view.ViewLike

import scala.xml.Node


object BackgroundColor {

  var read:(Node) => Option[Color] = { _ => None }

  def unapply(view: ViewLike): Option[Color] = {
    (view.xml \ "color").filter { _.attributes.asAttrMap.get("key") == Some("backgroundColor") }.headOption.flatMap { c =>
      read(c) match {
        case Some(x) => Some(x)
        case _ => RGBColor.unapply(c)
      }
    }
  }

}