import xibss.constraint.Constraint

import scala.xml.Node

package object xibss {

  type NodeAttributes = Map[String, String]

  case class Color(red: Double, green: Double, blue: Double, alpha: Double) {

    val alphaRounded = Math.round(alpha * 100) / 100.0
    def toCSS = s"rgba(${Math.round(red * 255).toInt}, ${Math.round(green * 255).toInt}, ${Math.round(blue * 255).toInt}, ${alphaRounded})"

  }

  trait Attribute[T] {


  }


  case class ViewNode(baseClass: String, id: String,  xml: Node, parentId: Option[String] = None, attributes: NodeAttributes = Map.empty, customClass: Option[String] = None, subviews: List[ViewNode] = List.empty, constraints: List[Constraint] = List.empty)


  implicit class ViewNodeExtensions(self: ViewNode) {

    /**
     * Performs the a traversal of a view
     *
     * @param parent a starting parent, you never need to provide a parent
     * @param breadthFirst if true, breadth-first traversal, otherwise, depth-first traversal
     * @param handler a callback for each time a node is visited
     */
    def walk(breadthFirst: Boolean = true, parent: Option[ViewNode] = None)(handler: (ViewNode, Option[ViewNode], List[ViewNode]) => Unit) {
      val siblings = parent.map { _.subviews }.getOrElse(List.empty).filter { _.id != self.id }
      if(breadthFirst) {
        handler(self, parent, siblings)
      }
      self.subviews.foreach { s =>
        s.walk(breadthFirst, Some(self))(handler)
      }
      if(!breadthFirst) {
        handler(self, parent, siblings)
      }
    }

  }

  implicit class GSSConstraintTranslations(self: String) {

    def toGSSName: String = {
      self match {
        case "leading" => "left"
        case "trailing" => "right"
        case "centerX" => "center-x"
        case "centerY" => "center-y"
        case x => x
      }
    }

  }

}