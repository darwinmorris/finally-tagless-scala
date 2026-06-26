package taglessFinal

import org.scalatest.funsuite.AnyFunSuite
import taglessFinal.Code.*

class PartialEvaluatorTests extends AnyFunSuite:

  def staticAdd[Repr[_, _]](using S: SymanticsPE[Repr]): Repr[Int, Int] =
    import S.*
    add(int(1), int(2))

  def addZero[Repr[_, _]](using S: SymanticsPE[Repr]): Repr[Int, Int] =
    import S.*
    add(int(0), int(5))

  def mulOne[Repr[_, _]](using S: SymanticsPE[Repr]): Repr[Int, Int] =
    import S.*
    mul(int(1), int(7))

  def staticIf[Repr[_, _]](using S: SymanticsPE[Repr]): Repr[Int, Int] =
    import S.*
    _if(bool(true))(int(10))(int(20))

  def identityBool[Repr[_, _]](using S: SymanticsPE[Repr]): Repr[Boolean, Boolean] =
    import S.*
    app(lam((x: Repr[Boolean, Boolean]) => x), bool(true))

  test("partial evaluator statically evaluates addition") {
    val pe = PartialEvaluator()
    val result = staticAdd(using pe.partialEvaluatorSymantics)

    assert(result.st.contains(3))
    assert(result.dy == IntC(3))
  }

  test("partial evaluator eliminates addition by zero") {
    val pe = PartialEvaluator()
    val result = addZero(using pe.partialEvaluatorSymantics)

    assert(result.st.contains(5))
    assert(result.dy == IntC(5))
  }

  test("partial evaluator eliminates multiplication by one") {
    val pe = PartialEvaluator()
    val result = mulOne(using pe.partialEvaluatorSymantics)

    assert(result.st.contains(7))
    assert(result.dy == IntC(7))
  }

  test("partial evaluator chooses statically known if branch") {
    val pe = PartialEvaluator()
    val result = staticIf(using pe.partialEvaluatorSymantics)

    assert(result.st.contains(10))
    assert(result.dy == IntC(10))
  }

  test("partial evaluator beta-reduces identity application") {
    val pe = PartialEvaluator()
    val result = identityBool(using pe.partialEvaluatorSymantics)

    assert(result.st.contains(true))
    assert(result.dy == BoolC(true))
  }

  test("partial evaluator residualizes dynamic addition") {
    val pe = PartialEvaluator()
    val S = pe.partialEvaluatorSymantics

    val x: PE[Int, Int] =
      PE(None, VarC[Int]("x"))

    val result =
      S.add(x, S.int(1))

    assert(result.st.isEmpty)
    assert(result.dy == AddC(VarC[Int]("x"), IntC(1)))
  }

  test("partial evaluator simplifies dynamic addition by zero") {
    val pe = PartialEvaluator()
    val S = pe.partialEvaluatorSymantics

    val x: PE[Int, Int] =
      PE(None, VarC[Int]("x"))

    val result =
      S.add(x, S.int(0))

    assert(result.st.isEmpty)
    assert(result.dy == VarC[Int]("x"))
  }

  test("partial evaluator beta-reduces with dynamic argument") {
    val pe = PartialEvaluator()
    val S = pe.partialEvaluatorSymantics

    val x: PE[Int, Int] =
      PE(None, VarC[Int]("x"))

    val id =
      S.lam[Int, Int](arg => arg)

    val result =
      S.app(id, x)

    assert(result.st.isEmpty)
    assert(result.dy == VarC[Int]("x"))
  }