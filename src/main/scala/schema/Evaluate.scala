package schema
import semantics._
import syntax.{Expression as Expr, Constant, Datum}

def eval(expr: Expr, env: MutableEnvironment): Computation = {
  expr match {
    case const: Expr.Const                  => eval_const(const)
    case variable: Expr.Var                 => eval_var(variable, env)
    case call: Expr.ProcedureCall           => eval_call(call, env)
    case lambda: Expr.LambdaExpr            => eval_lambda(lambda, env)
    case cond: Expr.Conditional             => eval_cond(cond, env)
    case cond_noAlt: Expr.Conditional_NoAlt => eval_condNoAlt(cond_noAlt, env)
    case assign: Expr.Assignment            => eval_assign(assign, env)
    case definition: Expr.Definition        => eval_def(definition, env)
    case quote: Expr.Quote                  => eval_quote(quote)
  }
}

def eval_quote(quote: Expr.Quote): Computation = {
  quote.datum match {
    case Datum.Bool(v)   => ExpressedValue.Bool(v)
    case Datum.Num(v)    => ExpressedValue.Num(v)
    case Datum.Char(c)   => ExpressedValue.Char(c)
    case Datum.String(s) => ExpressedValue.String(s)
    case Datum.Symbol(v) => ExpressedValue.Symbol(v)
    // compound quotes are evaluated to pairs
    case Datum.Compound(datums) =>
      datums match {
        case Nil => ExpressedValue.Nil
        case first :: rest =>
          ExpressedValue.Pair(
            eval_quote(Expr.Quote(first)),
            eval_quote(Expr.Quote(Datum.Compound(rest)))
          )
      }
  }
}

def eval_const(const: Expr.Const): Computation = {
  const.value match {
    case Constant.Bool(isTrue) => ExpressedValue.Bool(isTrue)
    case Constant.Char(c)      => ExpressedValue.Char(c)
    case Constant.Num(n)       => ExpressedValue.Num(n)
    case Constant.String(s)    => ExpressedValue.String(s)
    case Constant.Nil          => ExpressedValue.Nil
  }
}

def eval_var(`var`: Expr.Var, env: MutableEnvironment): Computation = {
  env.lookup(`var`.id) match {
    case Some(value) => value
    case None        => scala.sys.error(f"Variable ${`var`.id} is undefined")
  }
}

def eval_call(
    call: Expr.ProcedureCall,
    env: MutableEnvironment
): Computation = {
  val operator = eval(call.operator, env) match {
    case ExpressedValue.Procedure(f) => f
    case sthElse                     => sys.error(f"'${sthElse}' is not a procedure")
  }
  val operands = call.operands.map { operand => eval(operand, env) }
  operator(operands)
}

// this is harder to understand; pls refer to DCPL chap 6.5
def eval_lambda(
    lambda: Expr.LambdaExpr,
    env: MutableEnvironment
): Computation = {
  val Expr.LambdaExpr(ids, commands, returnValue) = lambda
  val f: (List[ExpressedValue]) => Computation = (operands) => {
    val bindings = (ids zip operands).toMap
    // This `env` would be modified if later a `define` is evaluated,
    // and that's why recursion can be handled
    //
    // Actually, this behavior is just a trick that my implementation of scheme is
    // dynamically scoped, so when a free variable occurs in a lambda definition it
    // will not use the `environment` determined at the time of definition.
    // See this pic: https://tva1.sinaimg.cn/large/008i3skNgy1gr1l97iecdj30vq0u0gwi.jpg
    val newEnv = env.extend(bindings)
    commands.foreach { cmd => eval(cmd, newEnv) }
    eval(returnValue, newEnv)
  }
  ExpressedValue.Procedure(f)
}

def eval_cond(cond: Expr.Conditional, env: MutableEnvironment): Computation = {
  val Expr.Conditional(test, consequent, alt) = cond
  val isTrue = eval(test, env) match {
    case ExpressedValue.Bool(v) => v
    case sthElse                => sys.error(f"$sthElse is not a boolean")
  }
  if isTrue then eval(consequent, env) else eval(alt, env)
}

def eval_condNoAlt(
    condNoAlt: Expr.Conditional_NoAlt,
    env: MutableEnvironment
): Computation = { ExpressedValue.Bool(true) }

def eval_assign(
    assign: Expr.Assignment,
    env: MutableEnvironment
): Computation = {
  ExpressedValue.Bool(true)
}

// One of the little portion of rules that mutates the environment
def eval_def(`def`: Expr.Definition, env: MutableEnvironment): Computation = {
  env.addEntry(`def`.id, eval(`def`.value, env))
  ExpressedValue.Symbol(`def`.id)
}
