package schema

import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional

enum TokenID {
  case Keyword(name: String)
  case Var(id: String)
}

enum Token extends Positional {
  case ID(kind: TokenID)
  case Bool(isTrue: Boolean)
  case Num(value: BigDecimal)
  case Char(c: scala.Char)
  case String(s: scala.Predef.String)
  case LParen, RParen, Apostrophe, BackQuote, Comma, Dot
  case HashLParen /* #( */
  case CommaAtSign /* ,@ */
}

object Lexer extends JavaTokenParsers {
  // Don't skip whitespace by default, otherwise each SUBPARSER will skip
  // whitespaces before they're run. These subparsers are just components
  // of main parsers which we really care and which need skipping whitespaces.
  override def skipWhitespace = false

  def token: Parser[Token] = positioned {
    identifier | syntacticKeyword | boolean | number | character | string
      | "(" ^^ (_ => Token.LParen)
      | ")" ^^ (_ => Token.RParen)
      | "'" ^^ (_ => Token.Apostrophe)
      | "`" ^^ (_ => Token.BackQuote)
      | "," ^^ (_ => Token.Comma)
      | withDelimiterAfter { "." ^^ (_ => Token.Dot) }
      | "(#" ^^ (_ => Token.HashLParen)
      | ",@" ^^ (_ => Token.CommaAtSign)
  }

  /// Returning `Unit`` means we don't care what they return
  def delimiter: Parser[Unit] = """[ \(\)";]|\Z""".r ^^^ ()
  def withDelimiterAfter[T](parser: Parser[T]) = parser <~ not(not(delimiter))

  def whitespace: Parser[Unit] = """[ \n]""".r ^^^ ()
  // Without the \n in the regex, comment would not take effect
  // consider an example stream `;3`
  def comment: Parser[Unit] = """;[^\n]*""".r ^^^ ()
  def atmosphere: Parser[Unit] = (whiteSpace | comment) ^^^ ()
  def intertokenSpace: Parser[Unit] = this.rep(atmosphere) ^^^ ()

  def identifier: Parser[Token] = variable | syntacticKeyword

  def initial: Parser[String] = letter | specialInitial
  def letter: Parser[String] = "[a-zA-Z]".r
  def specialInitial: Parser[String] = "[!$%&*/:<=>?^_~]".r

  def subsequent: Parser[String] = initial | digit | specialSubsequent
  def digit: Parser[String] = "[0-9]".r
  def specialSubsequent: Parser[String] = """[+-\.@]""".r

  // escape the dot!
  def peculiarIdentifier: Parser[String] = """[+-]|(\.\.\.)""".r

  def syntacticKeyword: Parser[Token] =
    (expressionKeyword | "(else)|(=>)|(define)|(unquote)|(unquote-splicing)".r | "nil") ^^ {
      name => Token.ID(TokenID.Keyword(name))
    }
  def expressionKeyword: Parser[String] =
    "(quote)|(lambda)|(if)|(set!)|(begin)|(cond)|(and)|(or)|(case)|(let)|(let\\*)|(letrec)|(do)|(delay)|(quasiquote)".r

  def variable: Parser[Token] = positioned {
    this.not(syntacticKeyword) ~> withDelimiterAfter {
      (initial ~ this.rep(subsequent)) ^^ { case initial ~ subsequents =>
        Token.ID(TokenID.Var(initial + subsequents.mkString))
      }
        | peculiarIdentifier ^^ { name => Token.ID(TokenID.Var(name)) }
    }
  }

  def boolean: Parser[Token] = positioned {
    "#t" ^^ (_ => Token.Bool(true))
      | "#f" ^^ (_ => Token.Bool(false))
  }

  def character: Parser[Token] = positioned {
    withDelimiterAfter {
      """#\""" ~> characterName ^^ (c => Token.Char(c))
        | """#\\.""".r ^^ (s => Token.Char(s.charAt(2)))
    }
  }

  def characterName: Parser[Char] =
    "space" ^^ (_ => ' ') | "newline" ^^ (_ => '\n')

  def string: Parser[Token] = positioned {
    "\"" ~> this.rep(stringElement) <~ "\"" ^^ { strings =>
      Token.String(strings.mkString)
    }
  }

  def stringElement: Parser[String] = """[^"\\]|(\\")|(\\\\f)""".r

  def number = positioned {
    withDelimiterAfter {
      """[+-]?(\d+(\.\d*)?|\d*\.\d+)""".r ^^ (d => Token.Num(BigDecimal(d)))
    }
  }

  def tokens: Parser[List[Token]] =
    this.rep1(intertokenSpace ~> token) <~ intertokenSpace

  def apply(code: String) = parseAll(tokens, code) match
    case Success(result, _) => result
    case failure    => scala.sys.error(failure.toString)
}
