package schema

import scala.util.parsing.combinator._
import scala.util.parsing.input.{Reader, Position, NoPosition}
import syntax._

object Parser extends Parsers {
  override type Elem = Token

  class TokenReader(tokens: List[Token]) extends Reader[Token] {
    override def first = tokens.head
    override def atEnd = tokens.isEmpty
    override def pos = tokens.headOption.map(_.pos).getOrElse(NoPosition)
    override def rest = TokenReader(tokens.tail)
  }

  // A simplified node for definition, which may be improved in future
  def definition = Token.LParen ~> define ~> variable ~ expression <~ Token.RParen ^^ {
    case variable ~ expression => Expression.Definition(variable, expression)
  }
  def define = accept("The 'define' keyword", { case Token.ID(TokenID.Keyword("define")) => () })

  // def program: Parser[SchemeProgram] = phrase(expression) ^^ { case expression => List(expression) }
  def program = phrase(expression | definition)

  def expression = positioned {
    variable ^^ {
      Expression.Var(_)
    } | literal | procedureCall | lambdaExpression | conditional
  }

  def literal = positioned {
    selfEvaluating
  }

  def selfEvaluating = positioned {
    boolean | number | character | string
  }

  def procedureCall = positioned {
    Token.LParen ~> operator ~ operand.* <~ Token.RParen ^^ {
      case operator ~ operands => Expression.ProcedureCall(operator, operands)
    }
  }

  def operator: Parser[Expression] = positioned {
    expression
  }

  def operand: Parser[Expression] = positioned {
    expression
  }

  def lambdaExpression = positioned {
    Token.LParen ~> lambda ~> formals ~ body <~ Token.RParen ^^ {
      case formals ~ body =>
        Expression.LambdaExpr(formals, body.init, body.last)
    }
  }

  def formals = {
    Token.LParen ~> variable.* <~ Token.RParen
      | variable ^^ (v => List(v))
      | Token.LParen ~> variable.+ <~ Token.Dot ~ variable <~ Token.RParen
  }

  def body = sequence

  // DON'T assign sequence to `command.* ~ expression`, which would cause
  // left recursion as here `command == expression`. In EBNF, patterns like
  // A* A would cause left recursion.
  def sequence: Parser[List[Expression]] = expression.+ //^^ {
  //   case commands ~ expression => commands :+ expression
  // }

  def command = expression

  def conditional = Token.LParen ~> `if` ~> test ~ consequent ~ alternate <~ Token.RParen ^^ {
    case test ~ consequent ~ alternate => Expression.Conditional(test, consequent, alternate)
  }
  def test: Parser[Expression] = expression
  def consequent: Parser[Expression] = expression
  def alternate: Parser[Expression] = expression
  def `if` = accept("'if' keyword", { case Token.ID(TokenID.Keyword("if")) => () })

  def variable: Parser[Identifier] =
    accept("Variable", { case Token.ID(TokenID.Var(name)) => name })

  def lambda: Parser[Unit] = accept(
    "The 'lambda' keyword",
    { case Token.ID(TokenID.Keyword(name)) if name == "lambda" => () }
  )

  def boolean: Parser[Expression] = positioned {
    accept(
      "Boolean",
      { case Token.Bool(v) => Expression.Const(Constant.Bool(v)) }
    )
  }

  def number: Parser[Expression] = positioned {
    accept("Number", { case Token.Num(v) => Expression.Const(Constant.Num(v)) })
  }

  def character: Parser[Expression] = positioned {
    accept(
      "Character",
      { case Token.Char(c) => Expression.Const(Constant.Char(c)) }
    )
  }

  def string: Parser[Expression] = positioned {
    accept(
      "String",
      { case Token.String(s) => Expression.Const(Constant.String(s)) }
    )
  }

  def apply(tokens: List[Token]): Expression = {
    val reader = TokenReader(tokens)
    program(reader) match {
      case Success(result, _) => result
      case failure            => scala.sys.error(failure.toString)
    }
  }

  def apply(code: String): Expression =
    val tokens = Lexer(code)
    val AST = apply(tokens)
    AST
}
