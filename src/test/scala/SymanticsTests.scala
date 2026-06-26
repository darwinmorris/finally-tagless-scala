package taglessFinal

import org.scalatest.funsuite.AnyFunSuite

class SymanticsTests extends AnyFunSuite:

  def arith[Repr[_]](using S: Symantics[Repr]): Repr[Int] =
    import S.*
    add(int(1), mul(int(2), int(3)))

  def identityBool[Repr[_]](using S: Symantics[Repr]): Repr[Boolean] =
    import S.*
    app(lam((x: Repr[Boolean]) => x), bool(true))

  def conditional[Repr[_]](using S: Symantics[Repr]): Repr[Int] =
    import S.*
    if_(leq(int(1), int(2)), int(10), int(20))

  test("Id interpreter evaluates arithmetic") {
    assert(arith[Id] == 7)
  }

  test("Id interpreter evaluates lambda application") {
    assert(identityBool[Id])
  }

  test("Id interpreter evaluates if expression") {
    assert(conditional[Id] == 10)
  }

  test("Size interpreter counts arithmetic constructors") {
    assert(arith[Size] == 5)
  }

  test("Size interpreter counts lambda application constructors") {
    assert(identityBool[Size] == 3)
  }

  test("Size interpreter counts if expression constructors") {
    assert(conditional[Size] == 6)
  }