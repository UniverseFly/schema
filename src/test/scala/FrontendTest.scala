import org.scalatest.funsuite.AnyFunSuite

class LexerTest extends AnyFunSuite {
  test("Parser should parse basic scheme code") {
    SchemeParser("(1 2 3 4 5)")
    SchemeParser("()")
  }
}

