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
  def definition: Parser[Expression] =
    Token.LParen ~> define ~> variable ~ expression <~ Token.RParen ^^ {
      case variable ~ expression => Expression.Definition(variable, expression)
    }
    // sugared definition for lambda
      | (Token.LParen ~> define ~> Token.LParen ~> variable) ~ variable.* ~ (Token.RParen ~> body <~ Token.RParen) ^^ {
        case func ~ formals ~ body =>
          Expression.Definition(
            func,
            Expression.LambdaExpr(formals, body.init, body.last)
          )
      }

  def define = accept(
    "The 'define' keyword",
    { case Token.ID(TokenID.Keyword("define")) => () }
  )

  // def program: Parser[SchemeProgram] = phrase(expression) ^^ { case expression => List(expression) }
  def program = phrase(expression)

  def expression = positioned {
    variable ^^ {
      Expression.Var(_)
    } | literal | procedureCall | lambdaExpression
      | conditional | definition
  }

  def literal = positioned {
    selfEvaluating | quotation
  }

  def selfEvaluating = positioned {
    boolean ^^ { v => Expression.Const(Constant.Bool(v)) }
      | number ^^ { v => Expression.Const(Constant.Num(v)) }
      | character ^^ { c => Expression.Const(Constant.Char(c)) }
      | string ^^ { s => Expression.Const(Constant.String(s)) }
      | nil ^^^ Expression.Const(Constant.Nil)
      | emptyList ^^^ Expression.Const(Constant.Nil)
  }

  def nil = accept("'nil'", { case Token.ID(TokenID.Keyword("nil")) => () })
  def emptyList = Token.LParen ~ Token.RParen ^^^ ()

  def quotation: Parser[Expression] = positioned {
    (Token.LParen ~> quote ~> datum <~ Token.RParen
      | Token.Apostrophe ~> datum) ^^ { d => Expression.Quote(d) }
  }

  def quote =
    accept("'quote' keyword", { case Token.ID(TokenID.Keyword("quote")) => () })

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

  def conditional =
    Token.LParen ~> `if` ~> test ~ consequent ~ alternate <~ Token.RParen ^^ {
      case test ~ consequent ~ alternate =>
        Expression.Conditional(test, consequent, alternate)
    }
  def test: Parser[Expression] = expression
  def consequent: Parser[Expression] = expression
  def alternate: Parser[Expression] = expression
  def `if` =
    accept("'if' keyword", { case Token.ID(TokenID.Keyword("if")) => () })

  def variable: Parser[Identifier] =
    accept("Variable", { case Token.ID(TokenID.Var(name)) => name })

  def lambda: Parser[Unit] = accept(
    "The 'lambda' keyword",
    { case Token.ID(TokenID.Keyword(name)) if name == "lambda" => () }
  )

  def boolean: Parser[Boolean] = accept("Boolean", { case Token.Bool(v) => v })

  def number: Parser[BigDecimal] = accept("Number", { case Token.Num(v) => v })
  def character: Parser[Char] = accept("Character", { case Token.Char(c) => c })

  def string: Parser[String] = accept("String", { case Token.String(s) => s })

  def datum: Parser[Datum] = simpleDatum | compoundDatum

  // datum for quotation/list
  def simpleDatum: Parser[Datum] =
    boolean ^^ { v => Datum.Bool(v) }
      | number ^^ { v => Datum.Num(v) }
      | character ^^ { v => Datum.Char(v) }
      | string ^^ { v => Datum.String(v) }
      | symbol ^^ { v => Datum.Symbol(v) }

  def symbol = identifier

  def compoundDatum = list

  def list: Parser[Datum] = Token.LParen ~> datum.* <~ Token.RParen ^^ {
    datums => Datum.Compound(datums)
  }

  def identifier: Parser[String] = accept(
    "Identifier",
    {
      case Token.ID(TokenID.Var(name))     => name
      case Token.ID(TokenID.Keyword(name)) => name
    }
  )

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
