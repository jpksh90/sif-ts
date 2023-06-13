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
        if (expr.isEmpty || thenType.isEmpty || elseType.isEmpty) then None
        else if expr.get == TPLattice.Low || expr.get == thenType.get && thenType.get == elseType.get then expr // automatically encodes subtyping rule
        else None

      case While(e, b) =>
        val expr = exprType(e)
        val body = commandType(b)
        if expr.isEmpty || body.isEmpty then None
        else if expr.get == TPLattice.Low || expr.get == body.get then expr // automatically encodes subtyping rule
        else None


      case Sequence(c1, c2) =>
        val c1type = commandType(c1)
        val c2type = commandType(c2)
        c1type match
          case Some(t) => c2type match
            case Some(t1) => if t == t1 then c1type else Option(TPLattice.Low) // if c1 or c2 is typed in high then bring c1 or c2 to Low
            case None => None
          case None => None

  def isTypable: Boolean =
    val ctype = commandType(p.getRoot)
    println(s"${p.getRoot} \n typable in " + ctype)
    ctype.isDefined

  def isNotTypable: Boolean = !isTypable
}
