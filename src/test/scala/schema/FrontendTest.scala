package schema

import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite {
  test("Parser should parse basic scheme code") {
    Parser("(1 2 3 4 5)")
    assertThrows[AnyRef](Parser("()"))
  }
}

