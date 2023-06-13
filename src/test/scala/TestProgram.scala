import Op.Plus
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable
import scala.collection.immutable.HashMap

class TestProgram extends AnyFunSuite {
  test(" x=x+1 is typable ") {
    val x = Var("x")
    val p = Program(Assign(x, BinOp(x, Num(1), Plus)))
    val typingEnv : HashMap[Var, TPLattice]  = HashMap((x, TPLattice.Low));
    assert(NonInterference(p, typingEnv).isTypable)
  }

  test(" if (h) then l = l+1 else skip is not typable ") {
    val l = Var("l")
    val h = Var("h")
    val e = Assign(l, BinOp(l, Num(1), Plus))
    val ifThenElse = IfThenElse(h, e, Skip())
    val p = Program(ifThenElse)
    val typingEnv : HashMap[Var, TPLattice] = HashMap(l -> TPLattice.Low, h -> TPLattice.High)
    assert(NonInterference(p, typingEnv).isNotTypable)
  }

  test(" if (l) then h = h+1 else skip is typable ") {
    val l = Var("l")
    val h = Var("h")
    val e = Assign(h, BinOp(h, Num(1), Plus))
    val ifThenElse = IfThenElse(l, e, Skip())
    val p = Program(ifThenElse)
    val typingEnv: HashMap[Var, TPLattice] = HashMap(l -> TPLattice.Low, h -> TPLattice.High)
    assert(NonInterference(p, typingEnv).isTypable)
  }

  test(" l=h is not typable ") {
    val l = Var("l")
    val h = Var("h")
    val p = Program(Assign(l, h))
    val typingEnv: HashMap[Var, TPLattice] = HashMap(l -> TPLattice.Low, h -> TPLattice.High)
    assert(NonInterference(p, typingEnv).isNotTypable)
  }

  test(" while (l) h = h+1; is typable ") {
    val l = Var("l")
    val h = Var("h")
    val e = Assign(l, BinOp(l, Num(1), Plus))
    val ifThenElse = While(l, e)
    val p = Program(ifThenElse)
    val typingEnv: HashMap[Var, TPLattice] = HashMap(l -> TPLattice.Low, h -> TPLattice.High)
    assert(NonInterference(p, typingEnv).isTypable)
  }

  test(" while (h) l = l+1; is not typable ") {
    val l = Var("l")
    val h = Var("h")
    val e = Assign(l, BinOp(l, Num(1), Plus))
    val ifThenElse = While(h, e)
    val p = Program(ifThenElse)
    val typingEnv: HashMap[Var, TPLattice] = HashMap(l -> TPLattice.Low, h -> TPLattice.High)
    assert(NonInterference(p, typingEnv).isNotTypable)
  }

  test(" l=l+1; h=h+1 is typable ") {
    val l = Var("l")
    val h = Var("h")
    val e1 = Assign(l, BinOp(l, Num(1), Plus))
    val e2 = Assign(h, BinOp(h, Num(1), Plus))
    val p = Program(Sequence(e1, e2))
    val typingEnv: HashMap[Var, TPLattice] = HashMap(l -> TPLattice.Low, h -> TPLattice.High)
    assert(NonInterference(p, typingEnv).isTypable)
  }
}
