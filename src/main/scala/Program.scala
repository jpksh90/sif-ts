class Program(command: Command) {
  def getRoot: Command = command
}

enum Op(repr: String):
  override def toString: String = this.repr
  case Plus extends Op("+")
  case Minus extends Op("-")
  case Div extends Op("/")
  case Mul extends Op("*")
  case Gte extends Op(">=")
  case Lte extends Op("<=")
  case Gt  extends Op(">")
  case Lt extends Op("<")


/**
 * 2 point lattice L < H
 */

enum TPLattice(repr: String, value: Int):
  def v : Int = this.value;
  case Low extends TPLattice("LOW", 0)
  case High extends TPLattice("HIGH", 1)

object TPLattice {
  def join(l1: TPLattice, l2: TPLattice) : TPLattice = l1 match
    case Low => l2
    case High => High

  def lower(l1: TPLattice, l2: TPLattice) : Boolean = l1.v < l2.v

  def notLower(l1: TPLattice, l2: TPLattice) : Boolean = !lower(l1, l2)
}

abstract sealed class Expr {
    override def toString: String =
      this match {
        case Var(x) => x
        case Num(n) => String.valueOf(n)
        case BinOp(e1, e2, op) =>
          s"${e1.toString}$op${e2.toString}"
        case Declassify(e, l) => s"declassify(${e.toString()}, ${l})"
      }
}

case class Var(id: String) extends Expr
case class Num(num: Int) extends Expr
case class BinOp(e1: Expr, e2: Expr, op: Op) extends Expr
case class Declassify(e: Expr, l: TPLattice) extends Expr



sealed abstract class Command {
  override def toString: String = this match {
    case Assign(v, e) => s"${v.toString}:=${e.toString}"
    case Skip() => "skip"
    case IfThenElse(e, thenPart, elsePart) => s"if (${e.toString}) ${thenPart.toString} else ${elsePart.toString}"
    case While(e, body) => s"while (${e.toString}) { ${body.toString}"
    case Sequence(c1, c2) => s"${c1.toString}; ${c2.toString}"
  }
}

case class Assign(v: Var, e: Expr) extends Command
case class Skip() extends Command
case class IfThenElse(e: Expr, thenPart: Command, elsePart: Command) extends Command
case class While(e: Expr, body: Command) extends Command
case class Sequence(c1: Command, c2: Command) extends Command

