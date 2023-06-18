import org.scalatest.funsuite.AnyFunSuite

class TestInterpreter extends AnyFunSuite {

  var x: Var = Var("x")
  var y: Var = Var("y")
  var one: Num = Num(1)
  var zero: Num = Num(0)

  test("val x=1+1 evalautes x=2") {
    val program = Program(Assign(x, BinOp(one, one, Op.Plus)))
    val init = Map(x -> 0, y -> 1);
    val res = Interpreter(program, init).eval;
    assert(res == init + (x -> 2))
  }

  test("val x=y+1 evaluates x=4 when y=3") {
    val program = Program(Assign(x, BinOp(y, one, Op.Plus)))
    val init = Map(x -> 0, y -> 3);
    val res = Interpreter(program, init).eval;
    assert(res == init + (x -> 4))
  }

  test("x=y+1;y=y+1 evaluates x=4 when y=3") {
    val program = Program(Sequence(
      Assign(x, BinOp(y, one, Op.Plus)),
      Assign(y, BinOp(y, one, Op.Plus)))
    )
    val init = Map(x -> 0, y -> 3);
    val res = Interpreter(program, init).eval;
    assert(res == init + (x -> 4, y -> 4))
  }

  test("while (x > 0) x=x-1 terminates with x = 0") {
    val b = Assign(x, BinOp(x, one, Op.Minus))
    val e = BinOp(x, zero, Op.Gt)
    val progrm = Program(While(e, b))
    val init = Map(x -> 0, y -> 3);
    val res = Interpreter(progrm, init).eval;
    assert(res == init + (x->0))
  }

  test("while (x > 0) x=x+1 does not terminate") {
    val b = Assign(x, BinOp(x, one, Op.Plus))
    val e = BinOp(x, zero, Op.Gt)
    val progrm = Program(While(e, b))
    val init = Map(x -> 0, y -> 3);
    val res = Interpreter(progrm, init).eval;
    assert(res == init + (x -> 0))
  }
}
