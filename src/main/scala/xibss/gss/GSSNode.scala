package xibss.gss

import scala.collection.mutable.ListBuffer


case class GSSNode(className: Option[String] = None, children: ListBuffer[GSSNode] = ListBuffer.empty, attributes: ListBuffer[String] = ListBuffer.empty)
case class HTMLNode(className: Option[String] = None, var tag: Option[String] = None, var isContainer: Boolean = true, var inner: Option[String] = None, children: ListBuffer[HTMLNode] = ListBuffer.empty, var attributes: Map[String, String] = Map.empty)

object GSSNode {

  implicit class GSSNodeExtensions(self: GSSNode) {

    def toGSSString(rootClassName: String, builder: Option[StringBuilder] = None, depth: Int = 0): String = {
      val strBuilder = builder.getOrElse(new StringBuilder)
      val className = self.className.getOrElse(if(rootClassName.startsWith(".")) rootClassName else "." + rootClassName)
      val attributeIndent = "  " * (depth + 1)
      val classNameIndent =  attributeIndent.drop(2)
      strBuilder.append(s"\n${classNameIndent}${className} {\n")
      self.attributes.foreach { a =>
        strBuilder.append(s"${attributeIndent}${a};\n")
      }
      self.children.foreach { c =>
        c.toGSSString(rootClassName, Some(strBuilder), depth + 1)
      }
      strBuilder.append(s"${classNameIndent}}\n")
      strBuilder.toString
    }
  }

}

object HTMLNode {

  implicit class HTMLNodeExtensions(self: HTMLNode) {

    def toHTMLString(rootClassName: String, id: Option[String] = None, builder: Option[StringBuilder] = None, depth: Int = 0): String = {
      val strBuilder = builder.getOrElse(new StringBuilder)
      val className = self.className.getOrElse(if(rootClassName.startsWith(".")) rootClassName else "." + rootClassName)
      val htmlIndent = "  " * depth
      val tag = self.tag.getOrElse("div")
      val attributes = self.attributes.map { case (k, v) => s"${k}='${v}'" }.mkString(" ")
      val attributeStr = if(self.attributes.isEmpty) "" else " " + attributes

      strBuilder.append(s"${htmlIndent}<${tag} class='${className.drop(1)}'${attributeStr}")
      id.foreach { a =>
        strBuilder.append(s" id='${a}'")
      }
      if(!self.isContainer) {
        strBuilder.append(" />\n")
      }
      else {
        strBuilder.append(">\n")
        self.inner.foreach(a => strBuilder.append(s"${htmlIndent}  ${a}\n"))
        self.children.foreach { c =>
          c.toHTMLString(rootClassName, None, Some(strBuilder), depth + 1)
        }
        strBuilder.append(s"${htmlIndent}</${tag}>\n")
      }
      strBuilder.toString
    }
  }

}
