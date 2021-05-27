package schema

import scala.util.parsing.input.Positional

type Program = List[
  Expr | Def
]

enum Def {
  case VarDef(variable: Token.ID, value: Expr)
}

enum Literal:
  case Bool(isTrue: Boolean)
  case Num(value: BigDecimal)
  case Char(value: scala.Char)
  case String(value: scala.Predef.String)

enum Expr extends Positional {
  case Var(name: String)
  case Lit(value: Literal)
  // case Quote(expression: Expr)
  case ProcedureCall(operator: Expr, operands: List[Expr])
  case LambdaExpr(
      formals: List[Expr.Var],
      definitions: List[Def],
      expressions: List[Expr]
  )
  case Conditional(test: Expr, consequent: Expr, alternate: Option[Expr])
  case Assignment(variable: String, value: Expr)
}
