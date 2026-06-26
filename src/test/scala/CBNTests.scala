package taglessFinal

import org.scalatest.funsuite.AnyFunSuite

class CBNTests extends AnyFunSuite:

  def identityBool[Repr[_, _]](using S: SymanticsPE[Repr]): Repr[Boolean, Boolean] =
    import S.*
    app(lam((x: Repr[Boolean, Boolean]) => x), bool(true))

  def arithmetic[Repr[_, _]](using S: SymanticsPE[Repr]): Repr[Int, Int] =
    import S.*
    add(int(1), mul(int(2), int(3)))

  def conditional[Repr[_, _]](using S: SymanticsPE[Repr]): Repr[Int, Int] =
    import S.*
    _if(leq(int(1), int(2)))(int(10))(int(20))

  def diverg[Repr[_, _]](using S: SymanticsPE[Repr]): Repr[Int, Int] =
    import S.*
    app(
      lam((x: Repr[Int, Int]) => int(1)),
      app(fix[Int, Int](f => f()), int(2))
    )

  test("CBN CPS evaluates identity application") {
    assert(runCBN(identityBool(using cbnSymantics)) == true)
  }

  test("CBN CPS evaluates arithmetic") {
    assert(runCBN(arithmetic(using cbnSymantics)) == 7)
  }

  test("CBN CPS evaluates conditionals") {
    assert(runCBN(conditional(using cbnSymantics)) == 10)
  }

  test("CBN CPS does not evaluate unused divergent argument") {
    assert(runCBN(diverg(using cbnSymantics)) == 1)
  }