package schema
import semantics._
import syntax.{Expression as Expr, Constant}

def eval(expr: Expr, env: Environment): Computation = {
  expr match {
    case const: Expr.Const                  => eval_const(const, env)
    case variable: Expr.Var                 => eval_var(variable, env)
    case call: Expr.ProcedureCall           => eval_call(call, env)
    case lambda: Expr.LambdaExpr            => eval_lambda(lambda, env)
    case cond: Expr.Conditional             => eval_cond(cond, env)
    case cond_noAlt: Expr.Conditional_NoAlt => eval_condNoAlt(cond_noAlt, env)
    case assign: Expr.Assignment            => eval_assign(assign, env)
  }
}

def eval_const(const: Expr.Const, env: Environment): Computation = {
  const.value match {
    case Constant.Bool(isTrue) => ExpressedValue.Bool(isTrue)
    case Constant.Char(c)      => ExpressedValue.Char(c)
    case Constant.Num(n)       => ExpressedValue.Num(n)
    case Constant.String(s)    => ExpressedValue.String(s)
  }
}

def eval_var(`var`: Expr.Var, env: Environment): Computation = {
  env.lookup(`var`.id) match {
    case Some(expressedValue) => expressedValue
    case None => scala.sys.error(f"Variable ${`var`.id} is undefined")
  }
}

def eval_call(call: Expr.ProcedureCall, env: Environment): Computation = {
  ExpressedValue.Bool(true)
}

def eval_lambda(lambda: Expr.LambdaExpr, env: Environment): Computation = {
  ExpressedValue.Bool(true)
}
def eval_cond(cond: Expr.Conditional, env: Environment): Computation = {
  ExpressedValue.Bool(true)
}
def eval_condNoAlt(
    condNoAlt: Expr.Conditional_NoAlt,
    env: Environment
): Computation = { ExpressedValue.Bool(true) }
def eval_assign(assign: Expr.Assignment, env: Environment): Computation = {
  ExpressedValue.Bool(true)
}
