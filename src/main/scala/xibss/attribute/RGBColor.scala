package xibss.attribute

import xibss._

import scala.xml.Node


object RGBColor {


  def unapply(xml: Node): Option[Color] = {
    val attr = xml.attributes.asAttrMap
    List(tryColorSpace(attr, "colorSpace"), tryColorSpace(attr, "customColorSpace")).flatMap(a => a).headOption
  }

  def tryColorSpace(attr: Map[String, String], key: String) : Option[Color] = {
    attr.get(key).flatMap {
      case "calibratedRGB" =>
        Some(Color(attr("red").toDouble, attr("green").toDouble, attr("blue").toDouble, attr("alpha").toDouble))
      case "calibratedWhite" =>
        val alpha = attr.get("alpha").map {_.toDouble }.getOrElse(1.0)
        val white = attr("white").toDouble
        Some(Color(white, white, white, alpha))
      case x =>
        None
    }
  }

}