package schema
import semantics._
import syntax.{Expression as Expr, Constant}

def eval(expr: Expr, env: MutableEnvironment): Computation = {
  expr match {
    case const: Expr.Const                  => eval_const(const, env)
    case variable: Expr.Var                 => eval_var(variable, env)
    case call: Expr.ProcedureCall           => eval_call(call, env)
    case lambda: Expr.LambdaExpr            => eval_lambda(lambda, env)
    case cond: Expr.Conditional             => eval_cond(cond, env)
    case cond_noAlt: Expr.Conditional_NoAlt => eval_condNoAlt(cond_noAlt, env)
    case assign: Expr.Assignment            => eval_assign(assign, env)
    case definition: Expr.Definition        => eval_def(definition, env)
  }
}

def eval_const(const: Expr.Const, env: MutableEnvironment): Computation = {
  const.value match {
    case Constant.Bool(isTrue) => ExpressedValue.Bool(isTrue)
    case Constant.Char(c)      => ExpressedValue.Char(c)
    case Constant.Num(n)       => ExpressedValue.Num(n)
    case Constant.String(s)    => ExpressedValue.String(s)
  }
}

def eval_var(`var`: Expr.Var, env: MutableEnvironment): Computation = {
  env.lookup(`var`.id) match {
    case Some(expressedValue) => expressedValue
    case None => scala.sys.error(f"Variable ${`var`.id} is undefined")
  }
}

def eval_call(call: Expr.ProcedureCall, env: MutableEnvironment): Computation = {
  val operator = eval(call.operator, env) match {
    case ExpressedValue.Procedure(f) => f
    case sthElse => sys.error(f"'${sthElse}' is not a procedure")
  }
  val operands = call.operands.map { operand => eval(operand, env) }
  operator(operands)
}

// this is harder to understand; pls refer to DCPL chap 6.5
def eval_lambda(lambda: Expr.LambdaExpr, env: MutableEnvironment): Computation = {
  val Expr.LambdaExpr(ids, commands, returnValue) = lambda
  val f: (List[ExpressedValue]) => Computation = (operands) => {
    val bindings = (ids zip operands).toMap
    val newEnv = env.extend(bindings)
    commands.foreach { cmd => eval(cmd, newEnv) }
    eval(returnValue, newEnv)
  }
  ExpressedValue.Procedure(f)
}

def eval_cond(cond: Expr.Conditional, env: MutableEnvironment): Computation = {
  ExpressedValue.Bool(true)
}
def eval_condNoAlt(
    condNoAlt: Expr.Conditional_NoAlt,
    env: MutableEnvironment
): Computation = { ExpressedValue.Bool(true) }
def eval_assign(assign: Expr.Assignment, env: MutableEnvironment): Computation = {
  ExpressedValue.Bool(true)
}

// One of the little portion of rules that mutates the environment
def eval_def(`def`: Expr.Definition, env: MutableEnvironment): Computation = {
  env.addEntry(`def`.id, eval(`def`.value, env))
  null
}
