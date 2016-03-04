package xibss.attributes

import xibss._


trait Alpha extends Attribute[Double] {

  import Alpha._

  def resolve(attr: NodeAttributes) = read(attr)

}


object Alpha {

  var read:(NodeAttributes) => Option[Double] = { _ => None }

}