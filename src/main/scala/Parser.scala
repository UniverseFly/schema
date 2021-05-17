import scala.util.parsing.combinator._
import scala.util.parsing.input.{Reader, Position, NoPosition}

object Parser extends Parsers {
  override type Elem = Token

  class TokenReader(tokens: List[Token]) extends Reader[Token] {
    override def first = tokens.head
    override def atEnd = tokens.isEmpty
    override def pos = tokens.headOption.map(_.pos).getOrElse(NoPosition)
    override def rest = TokenReader(tokens.tail)
  }

  def program: Parser[Expr] = phrase(expression)

  def expression = variable | literal | procedureCall | lambdaExpression

  def literal = selfEvaluating

  def selfEvaluating = boolean | number | character | string

  def procedureCall = Token.LParen ~> operator ~ this.rep(operand) <~ Token.RParen ^^ { case operator ~ operands => Expr.ProcedureCall(operator, operands) }

  def operator: Parser[Expr] = expression

  def operand: Parser[Expr] = expression

  def lambdaExpression = Token.LParen  ~> formals ~ body ^^ { case formals ~ body => Expr.LambdaExpr(formals, Nil, body) }

  def formals = {
    Token.LParen ~> variable.* <~ Token.RParen
    | variable ^^ (v => List(v))
    | Token.LParen ~> variable.+ <~ Token.Dot
  }

  def body = sequence

  def sequence: Parser[List[Expr]] = command.* ~ expression ^^ { case commands ~ expression => commands :+ expression  }

  def command: Parser[Expr] = expression

  def variable: Parser[Expr.Var] =
    accept("Variable", { case Token.ID(name) => Expr.Var(name) })

  def lambda: Parser[Unit] = accept("The 'lambda' keyword", { case Token.ID(name) if name == "lambda" => () })

  def boolean: Parser[Expr] =
    accept("Boolean", { case Token.Bool(v) => Expr.Lit(Literal.Bool(v)) })

  def number: Parser[Expr] =
    accept("Number", { case Token.Num(v) => Expr.Lit(Literal.Num(v)) })

  def character: Parser[Expr] =
    accept("Character", { case Token.Char(c) => Expr.Lit(Literal.Char(c)) })

  def string: Parser[Expr] =
    accept("String", { case Token.String(s) => Expr.Lit(Literal.String(s)) })

  def apply(tokens: List[Token]) =
    val reader = TokenReader(tokens)
    program(reader)
}

object Both {
  def apply(code: String): Parser.ParseResult[Expr] =
    val tokens = Lexer(code)
    val ast = Parser(tokens)
    ast
}
