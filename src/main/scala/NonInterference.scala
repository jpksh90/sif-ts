import scala.collection.immutable.HashMap


class NonInterference(p: Program, typingEnv: HashMap[Var, TPLattice]) {

  private def exprType(e: Expr): Option[TPLattice] =
    val secType = e match
      case t: Var => typingEnv.getOrElse(t, throw new IllegalArgumentException("cannot find " + t))
      case Num(num) => TPLattice.Low
      case BinOp(e1, e2, op) => TPLattice.join(exprType(e1).get, exprType(e2).get)
      case Declassify(e, l) => exprType(e).getOrElse(throw new IllegalStateException("cannot type e"))
    Option(secType)


  private def commandType(c: Command): Option[TPLattice] =
    c match
      case Assign(v, e) =>
        val vtype = typingEnv.get(v)
        val etype = exprType(e)
        if vtype.isEmpty || etype.isEmpty then None
        else if vtype.get == etype.get then vtype
        else None

      case Skip() => Option(TPLattice.Low)

      case IfThenElse(e, thenPart, elsePart) =>
        val expr = exprType(e)
        val thenType = commandType(thenPart)
        val elseType = commandType(elsePart)
        expr match
          case Some(TPLattice.Low) => if thenType.isDefined && elseType.isDefined then Some(TPLattice.Low) else None
          case Some(TPLattice.High) => if thenType.contains(TPLattice.High) && elseType.contains(TPLattice.High) then Some(TPLattice.High) else None
          case None => None

      case While(e, b) =>
        val expr = exprType(e)
        val body = commandType(b)
        expr match
          case Some(TPLattice.Low) => if body.isDefined then expr else None
          case Some(TPLattice.High) => Some(TPLattice.High).filter(body.contains)
          case None => None

      case Sequence(c1, c2) =>
        val c1type = commandType(c1)
        val c2type = commandType(c2)
        c1type match
          case Some(t) => c2type match
            case Some(t1) => if t == t1 then c1type else Option(TPLattice.Low) // if c1 or c2 is typed in high then bring c1 or c2 to Low
            case None => None
          case None => None

  def getType: Option[TPLattice] = commandType(p.get)
  def isTypable: Boolean = getType.isDefined
  def isNotTypable: Boolean = !isTypable
}
