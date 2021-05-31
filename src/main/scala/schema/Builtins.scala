package schema

import semantics._

/// Built-in procedures
object Builtins {
  /// id -> procedure
  lazy val bindings = Map(
    "+" -> add,
    "-" -> minus,
    "*" -> mult,
    "/" -> div,
    "<" -> lt,
    "<=" -> le,
    ">" -> gt,
    ">=" -> ge,
    "=" -> eq,
  )

  def stdEnv = MutableEnvironment.emptyEnvironment.extend(bindings)

  def add = makeBinaryNumOp(ExpressedValue.Num.apply, (x, y) => x + y)
  def minus = makeBinaryNumOp(ExpressedValue.Num.apply, (x, y) => x - y)
  def mult = makeBinaryNumOp(ExpressedValue.Num.apply, (x, y) => x * y)
  def div = makeBinaryNumOp(ExpressedValue.Num.apply, (x, y) => x / y)
  def lt = makeBinaryNumOp(ExpressedValue.Bool.apply, (x, y) => x < y)
  def le = makeBinaryNumOp(ExpressedValue.Bool.apply, (x, y) => x <= y)
  def gt = makeBinaryNumOp(ExpressedValue.Bool.apply, (x, y) => x > y)
  def ge = makeBinaryNumOp(ExpressedValue.Bool.apply, (x, y) => x >= y)
  def eq = makeBinaryNumOp(ExpressedValue.Bool.apply, (x, y) => x == y)


  def makeBinaryNumOp[T](
      constructor: T => Computation,
      op: (BigDecimal, BigDecimal) => T
  ): Nameable = {
    val Num = ExpressedValue.Num
    val f: Procedure = args =>
      args match {
        case Num(a) :: Num(b) :: rest if rest.isEmpty =>
          constructor(op(a, b))
        case sthElse => sys.error("TODO")
      }
    ExpressedValue.Procedure(f)
  }
}
