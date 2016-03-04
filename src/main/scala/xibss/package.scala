package object xibss {

  type NodeAttributes = Map[String, String]

  case class Color(red: Double, green: Double, blue: Double, alpha: Double)

  trait Attribute[T] {

    def resolve(attrs: NodeAttributes): Option[T]

  }

  case class Constraint(firstItem: Option[String], firstAttribute: Option[String], secondItem: Option[String], secondAttribute: Option[String], constant: Double, multiplier: Double)
  case class ViewNode(baseClass: String, id: String, parentId: Option[String] = None, attributes: NodeAttributes = Map.empty, customClass: Option[String] = None, subviews: List[ViewNode] = List.empty, constraints: List[Constraint] = List.empty)



//  trait View extends
//  BackgroundColor with
//  Alpha
//  {
//
//    val subViews: List[View] = List.empty
//
//  }

}