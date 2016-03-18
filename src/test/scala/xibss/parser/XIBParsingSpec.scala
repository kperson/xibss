package xibss.parser

import xibss._

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.Stack

class XIBParsingSpec extends FlatSpec with  Matchers {

  behavior of ("XIBParser")

  it should "parse views" in {
    val view = <view id="1"></view>
    XIBParser.parse(view) should be (Some(ViewNode("UIView", "1")))
  }

  it should "parse labels" in {
    val view = <label id="2"></label>
    XIBParser.parse(view) should be (Some(ViewNode("UILabel", "2")))
  }

  it should "parse subviews" in {
    val view =
      <view id="1">
        <subviews>
          <label id="2" text="Hello World"></label>
        </subviews>
      </view>
    XIBParser.parse(view) should be (Some(ViewNode("UIView", "1", subviews = List(ViewNode("UILabel", "2", attributes = Map("text" -> "Hello World"), parentId = Some("1"))))))
  }

  it should "parse constraints" in {
    val view =
      <view id="1">
        <constraints>
          <constraint firstItem="1i" firstAttribute="1a" secondItem="2i" secondAttribute="2a" constant="4.1" multiplier="5.2" />
        </constraints>
      </view>
    XIBParser.parse(view) should be (Some(ViewNode("UIView", "1", constraints = List(Constraint(Some("1i"), Some("1a"), Some("2i"), Some("2a"), 4.1, 5.2)))))
  }

  it should "performs a breadth first traversal" in {
    val view =
      <view id="1">
        <subviews>
          <view id="2"></view>
        </subviews>
      </view>
    val node = XIBParser.parse(view).get
    var matchSequence = List("1", "2")
    node.walk(true) { (curr, parent, siblings) =>
      curr.id should be (matchSequence.head)
      matchSequence = matchSequence.drop(1)
    }
  }

  it should "performs a depth first traversal" in {
    val view =
      <view id="1">
        <subviews>
          <view id="2"></view>
        </subviews>
      </view>

    val node = XIBParser.parse(view).get
    var matchSequence = List("2", "1")
    node.walk(false) { (curr, parent, siblings) =>
      curr.id should be (matchSequence.head)
      matchSequence = matchSequence.drop(1)
    }
  }

  it should "parse a file" in {
    val view =
      <document>
        <objects>
          <view id="1"></view>
          <view id="2"></view>
        </objects>
      </document>
    val expected = List(ViewNode("UIView", "1"), ViewNode("UIView", "2"))
    XIBParser.parseFromFile(view) should be (expected)
  }

}
