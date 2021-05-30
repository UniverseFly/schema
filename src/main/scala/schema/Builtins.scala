package schema

import semantics._

/// Built-in procedures
object Builtins {
  /// id -> procedure
  lazy val bindings = Map(
    "+" -> add,
    "-" -> minus,
    "*" -> mult,
    "/" -> div
  )

  lazy val stdEnv: Environment = Environment.emptyEnvironment.extend(bindings)

  def add = makeBinaryNumOp((x, y) => x + y)
  def minus = makeBinaryNumOp((x, y) => x - y)
  def mult = makeBinaryNumOp((x, y) => x * y)
  def div = makeBinaryNumOp((x, y) => x / y)

  type BinOp[T] = (T, T) => T
  def makeBinaryNumOp(op: BinOp[BigDecimal]): Nameable = {
    def makeBinaryNumOp_rec(op: BinOp[BigDecimal]): Procedure = args =>
        args match {
          case args if args.length == 1 =>
            ExpressedValue.Num(convertToNumOrError(args(0)))
          case front :+ last => {
            val ExpressedValue.Num(lhs) =
              makeBinaryNumOp_rec(op)(front).asInstanceOf[ExpressedValue.Num]
            val rhs = convertToNumOrError(last)
            ExpressedValue.Num(op(lhs, rhs))
          }
          case _ => sys.error("Unreachable")
        }
    val f = (args: List[ExpressedValue]) =>
      if args.length < 2 then
        sys.error("The number of arguments should be no less than 2")
      else makeBinaryNumOp_rec(op)(args)
    ExpressedValue.Procedure(f)
  }

  def convertToNumOrError(arg: ExpressedValue): BigDecimal = arg match {
    case ExpressedValue.Num(d) => d
    case sthElse               => sys.error(f"$sthElse is not a number")
  }
}
