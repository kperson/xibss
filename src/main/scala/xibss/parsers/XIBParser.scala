package xibss.parsers

import xibss.{Constraint, ViewNode}

import scala.xml.{NodeSeq, Node}

object XIBParser {

  //these map to base classes in iOS
  val builtInTypes = Map (
    "label" -> "UILabel",
    "view" -> "UIView",
    "button" -> "UIButton"
  )

  /**
   * Parse views from a xib into a tree
   *
   * @param xml the root XML, this XML must be a UIView or subclass of UIView
   * @param parentId the parent id of the view's superview
   * @return a view node if successfully parsed
   */
  def parse(xml: Node, parentId: Option[String] = None): Option[ViewNode] = {
    //node name
    val label = xml.label
    val attributesExceptId = xml.attributes.asAttrMap.filterKeys { k => k != "id" }

    //id
    val id = (xml \ "@id").text
    builtInTypes.get(label).map { c =>

      //subviews
      val subviews = (xml \ "subviews").headOption.map { x =>
        x.child.flatMap(parse(_, Some(id)))
      }.getOrElse(List.empty).toList

      //constraints
      val constraints = (xml \ "constraints").headOption.map { x =>
        (x \ "constraint").map { a =>
          val firstItem = (a \ "@firstItem").textOptional
          val firstAttribute = (a \ "@firstAttribute").textOptional
          val secondItem = (a \ "@secondItem").textOptional
          val secondAttribute = (a \ "@secondAttribute").textOptional
          val constant = (a \ "@constant").textOptional.map { _.toDouble }.getOrElse(0.0)
          val multiplier = (a \ "@multiplier").textOptional.map { _.toDouble }.getOrElse(1.0)
          Constraint(firstItem, firstAttribute, secondItem, secondAttribute, constant, multiplier)
        }
      }.getOrElse(List.empty).toList

      ViewNode(c, id, parentId, attributes = attributesExceptId, subviews = subviews, constraints = constraints)
    }
  }

  implicit class NodeSeqExtensions(self: NodeSeq) {

    // instead of an empty string, we want an optional None
    def textOptional = if (self.isEmpty) None else Some(self.text)

  }

}
