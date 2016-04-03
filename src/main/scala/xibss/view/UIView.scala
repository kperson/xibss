package xibss.view

import xibss.ViewNode


case class UIView(node: ViewNode) extends ViewLike {

  def tag = "div"
  def tagIsContainer = true


}