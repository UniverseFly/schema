package schema

import org.scalatest.funsuite.AnyFunSuite
import semantics.ExpressedValue._

class EnvTest extends AnyFunSuite {
  test("Builtins should be recognized") {
    val env = Builtins.stdEnv
    assert(env.lookup("*") != None)
  }

  test("Mutating environment should be correct") {
    val env = Builtins.stdEnv
    val mult = env.lookup("*")
    eval(Parser("(define hello #t)"), env)
    assertResult(Some(Bool(true)))(env.lookup("hello"))
    assertResult(mult)(env.lookup("*"))
  }
}

class EvalTest extends AnyFunSuite {
  test("Comparison operators should be correctly evaluated") {
    val expr = Parser("(<= 1 2)")
    val env = Builtins.stdEnv
    assertResult(Bool(true))(eval(expr, env))
  }

  test("Procedure should be called correctly") {
    val expr = Parser("((lambda x (* 2 x)) 2)")
    val env = Builtins.stdEnv
    assertResult(Num(4))(eval(expr, env))
  }

  test("Should be correct after environment is mutated") {
    val env = Builtins.stdEnv
    env.addEntry("test", Num(5))
    val expr = Parser("(* 2 test)")
    assertResult(Num(10))(eval(expr, env))
  }
  
  test("Recursion should be handled correctly") {
    val env = Builtins.stdEnv
    val define =
      Parser("(define fact (lambda (x) (if (= x 0) 1 (* x (fact (- x 1))))))")
    val callFact = Parser("(fact 10)")
    eval(define, env)
    val result = eval(callFact, env)

    lazy val fact: (Int) => Int = x => if x == 0 then 1 else x * fact(x - 1)
    assertResult(semantics.ExpressedValue.Num(fact(10)))(result)
  }

  test("Empty list quotation as nil") {
    val env = Builtins.stdEnv
    val nil = Parser("'()")
    assertResult(semantics.ExpressedValue.Nil)(eval(nil, env))
  }

  test("Empty list as nil") {
    val env = Builtins.stdEnv
    val nil = Parser("()")
    assertResult(semantics.ExpressedValue.Nil)(eval(nil, env))
  }

  test("Nil should be evaluated correctly") {
    val env = Builtins.stdEnv
    val nil = Parser("nil")
  }
}
