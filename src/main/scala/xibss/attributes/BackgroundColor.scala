package xibss.attributes

import xibss._


trait BackgroundColor extends Attribute[Color] {

  import BackgroundColor._

  def resolve(attr: NodeAttributes) = read(attr)

}


object BackgroundColor {

  var read:(NodeAttributes) => Option[Color] = { _ => None }

}