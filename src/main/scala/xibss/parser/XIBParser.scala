package xibss.parser

import xibss.constraint.Constraint
import xibss.{ViewNode}

import scala.xml.{NodeSeq, Node}

object XIBParser {

  //these map to base classes in iOS
  val builtInTypes = Map (
    "label" -> "UILabel",
    "view" -> "UIView",
    "button" -> "UIButton",
    "imageView" -> "UIImageView"
  )

  /**
   * Parses a nib and extracts the views
   *
   * @param xml the rootXML of the nib
   * @return a list of views
   */
  def parseFromFile(xml: Node): List[ViewNode] = {
    builtInTypes.keys.flatMap { k =>
      val views = xml \ "objects" \ k
      views.flatMap { v => parse(v) }
    }.toList
  }

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

    def nodeName(str: String) = {
      "n_" + str.replaceAll("-", "_")
    }

    //id
    val id = nodeName((xml \ "@id").text)
    builtInTypes.get(label).map { c =>

      //subviews
      val subviews = (xml \ "subviews").headOption.map { x =>
        x.child.flatMap(parse(_, Some(id)))
      }.getOrElse(List.empty).toList

      val constraintExcludes = ((xml \ "variation" \ "mask").filter(_.attributes.asAttrMap("key") == "constraints")  \ "exclude" \ "@reference").map(_.text)
      //constraints
      val constraints = (xml \ "constraints").headOption.map { x =>
        (x \ "constraint").filter { x => !constraintExcludes.contains((x \ "@id").text) }.map { a =>
          val firstItem = (a \ "@firstItem").textOptional.map { nodeName(_) }
          val firstAttribute = (a \ "@firstAttribute").textOptional
          val secondItem = (a \ "@secondItem").textOptional.map { nodeName(_) }
          val secondAttribute = (a \ "@secondAttribute").textOptional
          val constant = (a \ "@constant").textOptional.map { _.toDouble }.getOrElse(0.0)
          val multiplier = (a \ "@multiplier").textOptional.map { _.toDouble }.getOrElse(1.0)
          val relation = (a \ "@relation").textOptional.getOrElse("equal")
          Constraint(firstItem, firstAttribute, secondItem, secondAttribute, constant, multiplier, id, relation)
        }
      }.getOrElse(List.empty).toList

      ViewNode(c, id, xml, parentId, attributes = attributesExceptId, subviews = subviews, constraints = constraints)
    }
  }

  implicit class NodeSeqExtensions(self: NodeSeq) {

    // instead of an empty string, we want an optional None
    def textOptional = if (self.isEmpty) None else Some(self.text)

  }

}
