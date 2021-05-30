package schema

import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite {
  test("Parser should parse lambda") {
    Parser("(1 2 3 4 5)")
    assertThrows[AnyRef](Parser("()"))
    assert(Parser("(lambda (x) x)").isInstanceOf[syntax.Expression.LambdaExpr])
  }

  test("Parser should parse definition") {
    assert(Parser("(define x 3)").isInstanceOf[syntax.Expression.Definition])
  }
}

