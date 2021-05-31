package schema

import org.scalatest.funsuite.AnyFunSuite
import syntax._

class ParserTest extends AnyFunSuite {
  test("Parser should parse lambda") {
    Parser("(1 2 3 4 5)")
    assertThrows[AnyRef](Parser("()"))
    assert(Parser("(lambda (x) x)").isInstanceOf[Expression.LambdaExpr])
  }

  test("Parser should parse definition") {
    assert(Parser("(define x 3)").isInstanceOf[Expression.Definition])
  }

  test("Parser should parse conditional") {
    assert(Parser("(if #t 1 2)").isInstanceOf[Expression.Conditional])
  }
}

