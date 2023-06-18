import Op.{Minus, Plus}

import scala.collection.immutable.HashMap

class Interpreter(program: Program, init: Map[Var, Int]) {

  private def exprEval(e: Expr, values: Map[Var, Int]): Int = {
    e match
      case BinOp(e1, e2, op) =>
        val e1val = exprEval(e1, values)
        val e2val = exprEval(e2, values)
        val v3 = op match
          case Op.Plus => e1val+e2val
          case Op.Minus => e1val-e2val
          case Op.Div => e1val/e2val
          case Op.Mul => e1val*e2val
          case Op.Gte => if e1val >= e2val then 1 else 0
          case Op.Lte => if e1val <= e2val then 1 else 0
          case Op.Gt => if e1val > e2val then 1 else 0
          case Op.Lt => if e1val < e2val then 1 else 0
          case Op.Neq => if !(e1val == e2val) then 1 else 0
          case Op.Eq => if e1val == e2val then 1 else 0
        v3
      case t: Var => values.getOrElse(t, throw IllegalArgumentException("variable not found"))
      case n: Num => n.num
      case default => throw IllegalStateException("not implemented")
  }

  private def commandEval(cmd: Command, values: Map[Var, Int]): Map[Var, Int] = {
    cmd match
      case While(e, body) =>
        if exprEval(e, values) == 0 then
          values
        else
          commandEval(body, values)
      case Assign(v, e) =>  values + (v -> exprEval(e, values))
      case Sequence(c1, c2) => commandEval(c2, commandEval(c1, values))
      case IfThenElse(e, thenPart, elsePart) => commandEval(elsePart, commandEval(thenPart, values))
      case Skip() => values
  }

  def eval : Map[Var, Int] = commandEval(program.get, init)
}
