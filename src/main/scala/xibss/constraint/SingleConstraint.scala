package xibss.constraint


object SingleConstraint {

  def unapply(constraint: Constraint) : Option[(String, Double)] = {
    if(constraint.firstAttribute.isDefined && constraint.secondAttribute == None) {
      Some(constraint.firstAttribute.get, constraint.constant)
    }
    else {
      None
    }
  }

}
