package xibss.parsers

import xibss._

import org.scalatest.{FlatSpec, Matchers}

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

}
