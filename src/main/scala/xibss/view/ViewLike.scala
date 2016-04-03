package xibss.view

import xibss.ViewNode
import xibss.gss.{HTMLNode, GSSNode}


trait ViewLike {

  import ViewLike._

  def tag: String
  def tagIsContainer: Boolean
  def innerHTML: Option[String] = None

  def node: ViewNode
  def xml = node.xml
  def id: String = node.id
  def extraAttributes: Map[String, Option[String]] = Map.empty

  val subviews = node.subviews.flatMap { _.toView }
  val constraints = node.constraints

  /**
   * Performs the a traversal of a view
   *
   * @param parent a starting parent, you never need to provide a parent
   * @param handler a callback for each time a node is visited
   */
  def walk(parent: Option[ViewLike] = None, depth: Int = 0)(handler: (ViewLike, Option[ViewLike], List[ViewLike], Int) => Unit) {
    val siblings = parent.map { _.subviews }.getOrElse(List.empty).filter { x => x.id != id }
    handler(this, parent, siblings, depth)

    subviews.foreach { s =>
      s.walk(Some(this), depth + 1)(handler)
    }
  }

  def extraGSS() : List[String] = List.empty

}

object ViewLike {

  implicit class ViewLikeExtensions(self: ViewLike) {

    def walkGSS(parent: Option[ViewLike] = None, rootNode: GSSNode)(handler: (ViewLike, Option[ViewLike], List[ViewLike], GSSNode, GSSNode) => Unit) {
      val meGSS = GSSNode(Some(s".${self.id}"))
      rootNode.children.append(meGSS)
      val siblings = parent.map {
        _.subviews
      }.getOrElse(List.empty).filter { x => x.id != self.id }
      handler(self, parent, siblings, meGSS, rootNode)

      self.subviews.foreach { s =>
        s.walkGSS(Some(self), meGSS)(handler)
      }
    }

    def walkHTML(parent: Option[ViewLike] = None, rootNode: HTMLNode)(handler: (ViewLike, Option[ViewLike], List[ViewLike], HTMLNode) => Unit) {
      val meHTML = HTMLNode(Some(s".${self.id}"))
      rootNode.children.append(meHTML)
      val siblings = parent.map {
        _.subviews
      }.getOrElse(List.empty).filter { x => x.id != self.id }
      handler(self, parent, siblings, meHTML)

      self.subviews.foreach { s =>
        s.walkHTML(Some(self), meHTML)(handler)
      }
    }

  }

  implicit class ViewNodeTransform(self: ViewNode) {

    def toView: Option[ViewLike] = {
      self.baseClass match {
        case "UIView" => Some(UIView(self))
        case "UILabel" => Some(UILabel(self))
        case "UIButton" => Some(UIButton(self))
        case "UIImageView" => Some(UIImageView(self))
        case _ => None
      }
    }

  }

}