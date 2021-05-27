package schema

import scala.util.parsing.input.Positional

enum Constant {
  case Bool(isTrue: Boolean)
  case Num(value: BigDecimal)
  case Char(value: scala.Char)
  case String(value: scala.Predef.String)
}

type Identifier = String

enum Expression extends Positional {
  case Const(value: Constant)
  case Var(id: Identifier)
  case ProcedureCall(operator: Expression, operands: List[Expression])
  case LambdaExpr(
      formals: List[Identifier],
      commands: List[Command],
      returnValue: Expression
  )
  case Conditional(
      test: Expression,
      consequent: Expression,
      alternate: Expression
  )
  case Conditional_NoAlt(test: Expression, consequent: Expression)
  case Assignment(id: Identifier, value: Expression)
}

type Command = Expression
