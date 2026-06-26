package taglessFinal

import org.scalatest.funsuite.AnyFunSuite
import taglessFinal.Var.*
import taglessFinal.Exp.*
import taglessFinal.U.*

class TagProblemTests extends AnyFunSuite:

  test("test1 evaluates to true") {
    assert(eval0(Nil, test1) == UB(true))
  }

  test("applying a boolean as a function fails at runtime") {
    val bad: Exp =
      A(B(true), B(false))

    assertThrows[RuntimeException] {
      eval0(Nil, bad)
    }
  }

  test("evaluating an open term fails at runtime") {
    val open: Exp =
      A(
        L(V(VS(VZ))),
        B(true)
      )

    assertThrows[MatchError] {
      eval0(Nil, open)
    }
  }

  test("lookup finds innermost variable") {
    assert(lookup(List("x", "y"), VZ) == "x")
  }

  test("lookup finds outer variable") {
    assert(lookup(List("x", "y"), VS(VZ)) == "y")
  }