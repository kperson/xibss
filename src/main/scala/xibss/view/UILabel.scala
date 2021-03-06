package xibss.view

import xibss._

import scala.xml.Node

case class UILabel(node: ViewNode) extends ViewLike {

  def tag = "label"
  def tagIsContainer = true
  override def innerHTML: Option[String] = List(UILabel.read(node.xml), node.attributes.get("text")).flatMap { a => a }.headOption

}

object UILabel {

  var read:(Node) => Option[String] = { _ => None }

}
