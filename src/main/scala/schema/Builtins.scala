package schema

import semantics.{
  ExpressedValue,
  MutableEnvironment,
  Computation,
  Nameable,
  Procedure
}

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
    "car" -> car,
    "cdr" -> cdr,
    "list" -> list,
    "cons" -> cons,
    "append" -> append,
    "length" -> length,
    "map" -> map,
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

  def append = ExpressedValue.Procedure(args => semantics.concatLists(args))

  def car = ExpressedValue.Procedure(args =>
    args match {
      case List(ExpressedValue.Pair(fst @ _, _)) => fst
      case _                                     => sys.error("ERROR: TODO")
    }
  )

  def cdr = ExpressedValue.Procedure(args =>
    args match {
      case List(ExpressedValue.Pair(_, scd @ _)) => scd
      case _                                     => sys.error("ERROR: TODO")
    }
  )

  def cons = ExpressedValue.Procedure(args =>
    args match {
      case List(fst, scd) => ExpressedValue.Pair(fst, scd)
      case _              => sys.error("ERROR: TODO")
    }
  )

  def length = ExpressedValue.Procedure(args =>
    args match {
      case List(ev) => semantics.length(ev)
      case _        => sys.error("ERROR: TODO")
    }
  )

  def list = ExpressedValue.Procedure(args => semantics.makeList(args))

  def map = ExpressedValue.Procedure(args => args match {
    case List(ExpressedValue.Procedure(proc), (list: ExpressedValue)) => 
      semantics.map(proc, list)
    case _ => sys.error("ERROR: TODO")
  })

  def makeBinaryNumOp[T](
      constructor: T => Computation,
      op: (BigDecimal, BigDecimal) => T
  ): Nameable = {
    val Num = ExpressedValue.Num
    val f: Procedure = args =>
      args match {
        case Num(a) :: Num(b) :: rest if rest.isEmpty =>
          constructor(op(a, b))
        case sthElse => sys.error("ERROR: TODO")
      }
    ExpressedValue.Procedure(f)
  }
}
