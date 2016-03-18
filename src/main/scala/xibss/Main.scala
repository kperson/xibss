package xibss

import java.io.File

import xibss.attribute._
import xibss.constraint._
import xibss.gss._
import xibss.gss.GSSNode._
import xibss.parser.XIBParser
import xibss.view._

import scala.collection.mutable.ListBuffer
import scala.xml.XML

object Main extends App {

  def adjustedFileName(raw: String) =  if (raw.startsWith("~" + File.separator)) System.getProperty("user.home") + raw.substring(1) else raw

  val command = args(0)
  val className = args(1)
  val id = if(command == "html" && args.length == 4) Some(args(2)) else None
  val mini = args(args.length - 1) == "mini"
  val file = adjustedFileName(if(mini) args(args.length - 2) else args(args.length - 1))

  import ViewLike._

  val nib = XML.loadFile(file)
  val nodes = XIBParser.parseFromFile(nib)
  val views = nodes.flatMap { _.toView }


  views.foreach { v =>
    val rootGSS = GSSNode()
    val rootHTML = HTMLNode()
    v.walkGSS(rootNode = rootGSS) { (curr, parent, siblings, gss, parentGSS) =>
      if(parent.isEmpty) {
        gss.attributes.appendAll(List(
          "&[left] == ^[left]",
          "&[top] == ^[top]",
          "&[size] == ^[size]"
        ))
      }
      val constraintAttributes = curr.constraints.flatMap { a =>
        a match {
          case SingleConstraint(attribute, constant) =>
            List(s"${attribute}: ${a.relationSymbol} ${Math.round(constant)}")
          case x if x.firstItem.isEmpty =>
            if(x.constantAsInt == 0) {
              List(s".${x.secondItem.get}[${x.secondAttribute.get.toGSSName}] ${a.reverseRelationSymbol} &[${x.firstAttribute.get.toGSSName}]")
            }
            else {
              List(s".${x.secondItem.get}[${x.secondAttribute.get.toGSSName}] ${a.reverseRelationSymbol} &[${x.firstAttribute.get.toGSSName}] - ${x.constantAsInt}")
            }
          case x if Some(curr.id) == x.secondItem =>
            if(x.constantAsInt == 0) {
              List(s".${x.firstItem.get}[${x.firstAttribute.get.toGSSName}] ${a.relationSymbol} &[${x.secondAttribute.get.toGSSName}]")
            }
            else {
              List(s".${x.firstItem.get}[${x.firstAttribute.get.toGSSName}] ${a.relationSymbol} &[${x.secondAttribute.get.toGSSName}] + ${x.constantAsInt}")
            }
          case x if x.secondItem.isDefined =>
            if(x.constantAsInt == 0) {
              List(s".${x.firstItem.get}[${x.firstAttribute.get.toGSSName}] ${a.relationSymbol} .${x.secondItem.get}[${x.secondAttribute.get.toGSSName}]")
            }
            else {
              List(s".${x.firstItem.get}[${x.firstAttribute.get.toGSSName}]${a.relationSymbol} .${x.secondItem.get}[${x.secondAttribute.get.toGSSName}] + ${x.constantAsInt}")
            }
          case x => throw new RuntimeException(s"unable to match constraint: ${x}")
        }
      }

      gss.attributes.append("display: block")
      gss.attributes.appendAll(constraintAttributes.sortWith { (a, b) => a < b })

      BackgroundColor.unapply(curr).foreach { color =>
        gss.attributes.append(s"background-color: ${color.toCSS}")
      }

      TextColor.unapply(curr).foreach { color =>
        gss.attributes.append(s"color: ${color.toCSS}")
      }
    }

    v.walkHTML(rootNode = rootHTML) { (curr, parent, siblings, html) =>
      curr match {
        case UIView(node) =>
          html.tag = Some("div")
          html.isContainer = true
        case lbl @ UILabel(node) =>
          html.tag = Some("label")
          html.isContainer = true
          html.inner = lbl.text
        case btn @ UIButton(node) =>
          html.tag = Some("button")
          html.isContainer = true
          html.inner = btn.text
      }
    }

    if(command == "html") {
      if(mini) {
        println(rootHTML.toHTMLString(className, id).split("\n").map(_.trim).filter(!_.isEmpty).mkString(""))
      }
      else {
        println(rootHTML.toHTMLString(className, id))
      }
    }
    else if (command == "gss") {
      if(mini) {
        println(rootGSS.toGSSString(className).split("\n").map(_.trim).filter(!_.isEmpty).mkString(""))
      }
      else {
        println(rootGSS.toGSSString(className))
      }
    }
    else if(command == "preview") {
      val strBuilder = new ListBuffer[String]()
      strBuilder.append("<!DOCTYPE html>")
      strBuilder.append("<html lang=\"en\">")
      strBuilder.append("<head>")
      strBuilder.append("<script src=\"https://s3-us-west-2.amazonaws.com/cdn.thegrid.io/gss/v2.0.0/v2.0.0/gss.js\"></script>")
      strBuilder.append("<script type=\"text/javascript\"> window.engine = new GSS(document);</script>")
      strBuilder.append("<style type=\"text/gss\">")
      strBuilder.append(rootGSS.toGSSString("my_preview"))
      strBuilder.append(s".my_preview[width] == ${args(1)};")
      strBuilder.append(s".my_preview[height] == ${args(2)};")
      strBuilder.append(s".my_preview[top] == 0;")
      strBuilder.append(s".my_preview[left] == 0;")
      strBuilder.append("</style>")
      strBuilder.append("</head>")
      strBuilder.append("<body>")
      strBuilder.append(rootHTML.toHTMLString("my_preview"))
      strBuilder.append("</body>")
      strBuilder.append("</html>")
      println(strBuilder.mkString("\n"))
    }

  }

}