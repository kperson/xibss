package xibss.constraint


case class Constraint(firstItem: Option[String], firstAttribute: Option[String], secondItem: Option[String], secondAttribute: Option[String], constant: Double, multiplier: Double, constraintOwnerId: String, relation: String) {

  def constantAsInt: Int = Math.round(constant).toInt
  def relationSymbol = {
    relation match {
      case "greaterThanOrEqual" => ">="
      case "lessThanOrEqual" => "<="
      case _ => "=="
    }
  }

  def reverseRelationSymbol = {
    relation match {
      case "greaterThanOrEqual" => "<="
      case "lessThanOrEqual" => ">="
      case _ => "=="
    }
  }

}